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
import Rel8.Expr.Array ( sappend )
import Rel8.Kind.Emptiability ( SEmptiability( SNonEmptiable ) )
import Rel8.Schema.Context ( DB( DB ) )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Vectorize ( happend )
import Rel8.Table ( Table, Context, Columns, fromColumns, toColumns )
import Rel8.Table.Alternative ( AltTable, (<|>:) )


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
      (\nullability blueprint (DB a) (DB b) ->
         DB (sappend SNonEmptiable nullability blueprint a b))
      as
      bs
