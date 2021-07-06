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
import Prelude
import Data.List ( intercalate )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr( Expr, E, unE ) )
import Rel8.Expr.Array ( sappend, sempty, slistOf )
import Rel8.Expr.Opaleye ( unsafeLiteral, fromPrimExpr, toPrimExpr )
import Rel8.Schema.Dict ( Dict( Dict ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HTable, htabulate, hfield, hspecs, htraverse, hmap )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( happend, hempty, hvectorize, hunvectorize )
import Rel8.Schema.Name ( Name( N, Name, unN ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Result ( vectorizer, unvectorizer )
import Rel8.Schema.Spec ( Spec(..), SSpec(..) )
import Rel8.Table
  ( Table, Context, Columns, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Control.Monad.Trans.State.Strict ( evalState, get, put, State )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.HaskellDB.Sql.Default as O
import qualified Opaleye.Internal.HaskellDB.Sql.Print as O
import qualified Opaleye.Internal.HaskellDB.Sql.Generate as O
import Rel8.Table.Serialize ( ToExprs )


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'Rel8.many' or 'Rel8.listAgg'.
type ListTable :: K.Context -> Type -> Type
data ListTable context a where
  ListTable :: HTable f
    => (f context -> b)
    -> HListTable f context
    -> ListTable context b


instance Functor (ListTable i) where
  fmap f (ListTable g cols) = ListTable (f . g) cols


class ListContext context where
  mapColumns
    :: (HTable f, HTable g)
    => (f context -> g context)
    -> HListTable f context
    -> HListTable g context


instance ListContext Expr where
  mapColumns f i = hvectorize v $ pure $ htabulate \field ->
    case hfield hspecs field of
      SSpec{} ->
        hfield (f exprs) field
    where
      names = evalState (htraverse freshName i) 0

      namesList = getConst $ htraverse g names
        where
          g :: forall (a :: Spec). Name a -> Const [String] (Name a)
          g (N (Name x)) = Const [x]

      v :: SSpec ('Spec a) -> Identity (Expr ('Spec a)) -> Expr ('Spec [a])
      v SSpec{} (Identity (E (Expr yuck))) =
        E $ unsafeLiteral $
        "(SELECT array_agg(" <> show (O.ppSqlExpr (O.sqlExpr O.defaultSqlGenerator yuck)) <> ") FROM " <>
        "(SELECT " <> unnests <> ") q(" <> intercalate "," namesList <> "))"

      unnests :: String
      unnests = intercalate "," $ getConst $ htraverse unnest i
        where
          unnest :: forall (a :: Spec). Expr a -> Const [String] (Expr a)
          unnest (E (Expr yuck)) =
            Const [ "unnest(" <> show (O.ppSqlExpr (O.sqlExpr O.defaultSqlGenerator yuck)) <> ")" ]

      freshName :: forall (a :: Spec). Expr a -> State Int (Name a)
      freshName (E _) = do
        n <- get
        put (n + 1)
        return $ N $ Name $ "x" <> show n

      exprs = runIdentity $ hunvectorize uv (hmap projectName names)
        where
          uv :: SSpec ('Spec a) -> Expr ('Spec [a]) -> Identity (Expr ('Spec a))
          uv SSpec{} = pure . E . fromPrimExpr . toPrimExpr . unE

          projectName :: forall (a :: Spec). Name a -> Expr a
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


instance (Table context a, context ~ context', ListContext context') =>
  Table context' (ListTable context a)
 where
  type Columns (ListTable context a) = HListTable (Columns a)
  type Context (ListTable context a) = Context a
  type FromExprs (ListTable context a) = [FromExprs a]

  fromColumns = ListTable fromColumns
  toColumns (ListTable f cols) = mapColumns (toColumns . f) cols
  fromResult = fmap (fromResult @_ @a) . hunvectorize unvectorizer
  toResult = hvectorize vectorizer . fmap (toResult @_ @a)


instance (Recontextualize from to a b, from ~ from', to ~ to', ListContext from', ListContext to') =>
  Recontextualize from to (ListTable from' a) (ListTable to' b)


instance (EqTable a, context ~ Expr) => EqTable (ListTable context a) where
  eqTable =
    hvectorize
      (\SSpec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (eqTable @a))


instance (OrdTable a, context ~ Expr) => OrdTable (ListTable context a) where
  ordTable =
    hvectorize
      (\SSpec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (ordTable @a))


instance (ToExprs exprs a, context ~ Expr) =>
  ToExprs (ListTable context exprs) [a]


instance context ~ Expr => AltTable (ListTable context) where
  (<|>:) = (<>)


instance context ~ Expr => AlternativeTable (ListTable context) where
  emptyTable = mempty


instance (context ~ Expr, Table Expr a) =>
  Semigroup (ListTable context a)
 where
  as <> bs = fromColumns $
    happend (\_ _ (E a) (E b) -> E (sappend a b)) (toColumns as) (toColumns bs)


instance (context ~ Expr, Table Expr a) =>
  Monoid (ListTable context a)
 where
  mempty = fromColumns $ hempty $ \_ -> E . sempty


-- | Construct a @ListTable@ from a list of expressions.
listTable :: Table Expr a => [a] -> ListTable Expr a
listTable =
  fromColumns .
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
  fromColumns .
  hvectorize (\_ (Identity (N (Name a))) -> N (Name a)) .
  pure .
  toColumns
