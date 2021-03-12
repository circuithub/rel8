{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Table.List
  ( ListTable(..)
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Kind.Nullability ( withKnownNullability )
import Rel8.Schema.Context ( DB )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( happend, hempty )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Type ( withDBType )


type ListTable :: Type -> Type
newtype ListTable a = ListTable (HListTable (Columns a) (H (Context a)))


instance Table a => Table (ListTable a) where
  type Columns (ListTable a) = HListTable (Columns a)
  type Context (ListTable a) = Context a

  fromColumns = ListTable
  toColumns (ListTable a) = a


instance AltTable ListTable where
  (<|>:) = (<>)


instance AlternativeTable ListTable where
  emptyTable = mempty


instance (Table a, Context a ~ DB) => Semigroup (ListTable a) where
  ListTable as <> ListTable bs = ListTable $
    happend
      (\nullability info ->
        withKnownNullability nullability $
        withDBType info (<>))
      as
      bs


instance (Table a, Context a ~ DB) => Monoid (ListTable a) where
  mempty = ListTable $ hempty $ \nullability info ->
    withKnownNullability nullability $
    withDBType info mempty
