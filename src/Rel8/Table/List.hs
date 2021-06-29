{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.List
  ( ListTable(..)
  , listTable, nameListTable
  )
where

-- base
import Control.Applicative ( Const(..) )
import Data.Functor.Identity ( Identity( Identity ), runIdentity )
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude
import Data.List ( intercalate )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr(..), Col( E, unE ) )
import Rel8.Expr.Array ( sappend, sempty, slistOf )
import Rel8.Expr.Opaleye ( unsafeLiteral, fromPrimExpr, toPrimExpr )
import Rel8.Schema.Dict ( Dict( Dict ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HTable, htabulate, hfield, hspecs, htraverse, hmap )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( HVectorize(..), happend, hempty, hunvectorize, hvectorize )
import Rel8.Schema.Name ( Col( N ), Name( Name ), unN )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( Spec(..), SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType ( dbTypeDict, dbTypeNullity )
import Rel8.Schema.Reify ( UnwrapReify, hreify, hunreify )
import Rel8.Table
  ( Table, Context, Columns, Unreify, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
import Control.Monad.Trans.State.Strict ( evalState, get, put, State )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.HaskellDB.Sql.Default as O
import qualified Opaleye.Internal.HaskellDB.Sql.Print as O
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as O


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'Rel8.many' or 'Rel8.listAgg'.
type ListTable :: K.Context -> Type -> Type
data ListTable context a where
  ListTable :: HTable f
    => (f (Col context) -> b)
    -> HListTable f (Col context)
    -> ListTable context b


instance Functor (ListTable i) where
  fmap f (ListTable g cols) = ListTable (f . g) cols


class ListContext context where
  mapColumns
    :: (HTable f, HTable g)
    => (f (Col context) -> g (Col context))
    -> HListTable f (Col context)
    -> HListTable g (Col context)


instance ListContext Expr where
  mapColumns f i = hvectorize vectorizer $ pure $ htabulate \field ->
    case hfield hspecs field of
      SSpec{ info } ->
        hfield (f exprs) field
    where
      names = evalState (htraverse freshName i) 0

      namesList = getConst $ htraverse (\(N (Name x)) -> Const [x]) names

      vectorizer :: SSpec ('Spec a) -> Identity (Col Expr ('Spec a)) -> Col Expr ('Spec [a])
      vectorizer SSpec{info} (Identity (E (Expr yuck))) =
        E $ unsafeLiteral $
        "(SELECT array_agg(" <> show (O.ppSqlExpr (O.sqlExpr O.defaultSqlGenerator yuck)) <> ") FROM " <>
        "(SELECT " <> unnests <> ") q(" <> intercalate "," namesList <> "))"
      vectorizer SSpec{info} (Identity (E (Expr other))) = error $ show other

      unnests :: String
      unnests = intercalate "," $ getConst $ htraverse unnest i
        where
          unnest :: Col Expr a -> Const [String] (Col Expr a)
          unnest (E (Expr yuck)) =
            Const [ "unnest(" <> show (O.ppSqlExpr (O.sqlExpr O.defaultSqlGenerator yuck)) <> ")" ]

      freshName :: Col Expr a -> State Int (Col Name a)
      freshName (E _) = do
        n <- get
        put (n + 1)
        return $ N $ Name $ "x" <> show n

      exprs = runIdentity $ hunvectorize (\SSpec{info} -> pure . E . fromPrimExpr . toPrimExpr . unE) (hmap projectName names)
        where
          projectName :: Col Name a -> Col Expr a
          projectName (N (Name name)) =
            E $ fromPrimExpr $ O.BaseTableAttrExpr name


instance ListContext Aggregate where
  mapColumns f i = undefined


instance ListContext Name where
  mapColumns f =
    hvectorize (\_ -> rename . runIdentity) .
    fmap f .
    hunvectorize (\_ -> pure . rename)
    where rename = N . (\(Name x) -> Name x) . unN


instance (Table context a, c ~ context, Context a ~ c, ListContext context) =>
  Table context (ListTable c a)
 where
  type Columns (ListTable c a) = HListTable (Columns a)
  type Context (ListTable c a) = c
  type Unreify (ListTable c a) = ListTable (UnwrapReify c) (Unreify a)

  fromColumns c = ListTable fromColumns c

  toColumns (ListTable f cols) = mapColumns (toColumns . f . fromColumns) cols

  reify Refl (ListTable f cols) = ListTable (reify Refl . f . hunreify) (hreify cols)

  unreify Refl (ListTable f cols) = ListTable (unreify Refl . f . hreify) (hunreify cols)


instance
  ( Recontextualize from to a b
  , ListContext from, ListContext to
  , from ~ from'
  , to ~ to'
  )
  => Recontextualize from to (ListTable from' a) (ListTable to' b)


instance (context ~ Expr, EqTable a) => EqTable (ListTable context a) where
  eqTable =
    hvectorize
      (\SSpec {} (Identity dict) -> case dbTypeDict dict of
          Dict -> case dbTypeNullity dict of
            Null -> Dict
            NotNull -> Dict)
      (Identity (eqTable @a))


instance (context ~ Expr, OrdTable a) => OrdTable (ListTable context a) where
  ordTable =
    hvectorize
      (\SSpec {} (Identity dict) -> case dbTypeDict dict of
          Dict -> case dbTypeNullity dict of
            Null -> Dict
            NotNull -> Dict)
      (Identity (ordTable @a))


type instance FromExprs (ListTable _ a) = [FromExprs a]


instance (context ~ Expr, ToExprs exprs a) => ToExprs (ListTable context exprs) [a] where
  fromResult = fmap (fromResult @exprs) . fromColumns
  toResult = toColumns . fmap (toResult @exprs)


instance context ~ Expr => AltTable (ListTable context) where
  (<|>:) = (<>)


instance context ~ Expr => AlternativeTable (ListTable context) where
  emptyTable = mempty


instance (context ~ Expr, Table Expr a) => Semigroup (ListTable context a) where
  as <> bs = ListTable fromColumns $
    happend (\_ _ (E a) (E b) -> E (sappend a b)) (toColumns as) (toColumns bs)


instance (context ~ Expr, Table Expr a) => Monoid (ListTable context a) where
  mempty = ListTable fromColumns $ hempty $ \_ -> E . sempty


-- | Construct a @ListTable@ from a list of expressions.
listTable :: Table Expr a => [a] -> ListTable Expr a
listTable =
  ListTable fromColumns .
  hvectorize (\SSpec {info} -> E . slistOf info . fmap unE) .
  fmap toColumns


-- | Construct a 'ListTable' in the 'Name' context. This can be useful if you
-- have a 'ListTable' that you are storing in a table and need to construct a
-- 'TableSchema'.
nameListTable
  :: Table Name a
  => a -- ^ The names of the columns of elements of the list.
  -> ListTable Name a
nameListTable =
  ListTable fromColumns .
  hvectorize (\_ (Identity (N (Name a))) -> N (Name a)) .
  pure .
  toColumns
