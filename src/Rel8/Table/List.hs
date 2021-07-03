{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
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
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col( E, unE ) )
import Rel8.Expr.Array ( sappend, sempty, slistOf )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( happend, hempty, hvectorize )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Col( N ), Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Reify ( hreify, hunreify )
import Rel8.Table
  ( Table, Context, Columns, fromColumns, toColumns
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
import Rel8.Table.Unreify ( Unreifies )


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'Rel8.many' or 'Rel8.listAgg'.
type ListTable :: K.Context -> Type -> Type
newtype ListTable context a =
  ListTable (HListTable (Columns a) (Col (Context a)))


instance (Table context a, Unreifies context a, context ~ context') =>
  Table context' (ListTable context a)
 where
  type Columns (ListTable context a) = HListTable (Columns a)
  type Context (ListTable context a) = Context a

  fromColumns = ListTable
  toColumns (ListTable a) = a

  reify Refl (ListTable a) = ListTable (hreify a)
  unreify Refl (ListTable a) = ListTable (hunreify a)


instance
  ( Unreifies from a, from ~ from'
  , Unreifies to b, to ~ to'
  , Recontextualize from to a b
  )
  => Recontextualize from to (ListTable from' a) (ListTable to' b)


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


type instance FromExprs (ListTable _context a) = [FromExprs a]


instance (ToExprs exprs a, context ~ Expr) =>
  ToExprs (ListTable context exprs) [a]
 where
  fromResult = fmap (fromResult @exprs) . fromColumns
  toResult = toColumns . fmap (toResult @exprs)


instance context ~ Expr => AltTable (ListTable context) where
  (<|>:) = (<>)


instance context ~ Expr => AlternativeTable (ListTable context) where
  emptyTable = mempty


instance (context ~ Expr, Table Expr a) =>
  Semigroup (ListTable context a)
 where
  ListTable as <> ListTable bs = ListTable $
    happend (\_ _ (E a) (E b) -> E (sappend a b)) as bs


instance (context ~ Expr, Table Expr a) =>
  Monoid (ListTable context a)
 where
  mempty = ListTable $ hempty $ \_ -> E . sempty


-- | Construct a @ListTable@ from a list of expressions.
listTable :: Table Expr a => [a] -> ListTable Expr a
listTable =
  ListTable .
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
  ListTable .
  hvectorize (\_ (Identity (N (Name a))) -> N (Name a)) .
  pure .
  toColumns
