{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.NonEmpty
  ( NonEmptyTable(..)
  , HNonEmptyTable
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Array ( sappend1 )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Vectorize ( happend )
import Rel8.Table ( Table, Context, Columns, fromColumns, toColumns )
import Rel8.Table.Alternative ( AltTable, (<|>:) )
import Rel8.Table.Recontextualize ( Recontextualize )


-- | A @NonEmptyTable@ value contains one or more instances of @a@. You
-- construct @NonEmptyTable@s with 'some' or 'nonEmptyAgg'.
type NonEmptyTable :: Type -> Type
newtype NonEmptyTable a =
  NonEmptyTable (HNonEmptyTable (Columns a) (Col (Context a)))


instance Table context a => Table context (NonEmptyTable a) where
  type Columns (NonEmptyTable a) = HNonEmptyTable (Columns a)
  type Context (NonEmptyTable a) = Context a

  fromColumns = NonEmptyTable
  toColumns (NonEmptyTable a) = a


instance Recontextualize from to a b =>
  Recontextualize from to (NonEmptyTable a) (NonEmptyTable b)


instance AltTable NonEmptyTable where
  (<|>:) = (<>)


instance Table Expr a => Semigroup (NonEmptyTable a) where
  NonEmptyTable as <> NonEmptyTable bs = NonEmptyTable $
    happend (\_ _ (DB a) (DB b) -> DB (sappend1 a b)) as bs
