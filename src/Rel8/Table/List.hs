{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.List
  ( ListTable(..)
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Expr.Array ( sappend, sempty )
import Rel8.Schema.Context ( DB( DB ) )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( happend, hempty )
import Rel8.Table ( Table, Context, Columns, fromColumns, toColumns )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Recontextualize ( Recontextualize )


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'many' or 'listAgg'.
type ListTable :: Type -> Type
newtype ListTable a = ListTable (HListTable (Columns a) (H (Context a)))


instance Table context a => Table context (ListTable a) where
  type Columns (ListTable a) = HListTable (Columns a)
  type Context (ListTable a) = Context a

  fromColumns = ListTable
  toColumns (ListTable a) = a


instance Recontextualize from to a b =>
  Recontextualize from to (ListTable a) (ListTable b)


instance AltTable ListTable where
  (<|>:) = (<>)


instance AlternativeTable ListTable where
  emptyTable = mempty


instance Table DB a => Semigroup (ListTable a) where
  ListTable as <> ListTable bs = ListTable $
    happend (\_ _ (DB a) (DB b) -> DB (sappend a b)) as bs


instance Table DB a => Monoid (ListTable a) where
  mempty = ListTable $ hempty $ \nullability info ->
    DB (sempty nullability info)
