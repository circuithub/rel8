{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Table.NonEmpty
  ( NonEmptyTable(..)
  , HNonEmptyTable
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Kind.Nullability ( withKnownNullability )
import Rel8.Schema.Context ( DB )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Vectorize ( happend )
import Rel8.Table ( Table, Context, Columns, fromColumns, toColumns )
import Rel8.Table.Alternative ( AltTable, (<|>:) )
import Rel8.Type ( withDBType )


type NonEmptyTable :: Type -> Type
newtype NonEmptyTable a =
  NonEmptyTable (HNonEmptyTable (Columns a) (H (Context a)))


instance Table a => Table (NonEmptyTable a) where
  type Columns (NonEmptyTable a) = HNonEmptyTable (Columns a)
  type Context (NonEmptyTable a) = Context a

  fromColumns = NonEmptyTable
  toColumns (NonEmptyTable a) = a


instance AltTable NonEmptyTable where
  (<|>:) = (<>)


instance (Table a, Context a ~ DB) => Semigroup (NonEmptyTable a) where
  NonEmptyTable as <> NonEmptyTable bs = NonEmptyTable $
    happend
      (\nullability info ->
        withKnownNullability nullability $
        withDBType info (<>))
      as
      bs
