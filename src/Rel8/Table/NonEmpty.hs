{-# language FlexibleContexts #-}
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
import Rel8.Expr.Array ( sappend )
import Rel8.Schema.Context ( DB( DB ) )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Vectorize ( happend )
import Rel8.Table ( Table, Context, Columns, fromColumns, toColumns )
import Rel8.Table.Alternative ( AltTable, (<|>:) )
import Rel8.Table.Map ( MapTable )


type NonEmptyTable :: Type -> Type
newtype NonEmptyTable a =
  NonEmptyTable (HNonEmptyTable (Columns a) (H (Context a)))


instance Table context a => Table context (NonEmptyTable a) where
  type Columns (NonEmptyTable a) = HNonEmptyTable (Columns a)
  type Context (NonEmptyTable a) = Context a

  fromColumns = NonEmptyTable
  toColumns (NonEmptyTable a) = a


instance MapTable from to a b =>
  MapTable from to (NonEmptyTable a) (NonEmptyTable b)


instance AltTable NonEmptyTable where
  (<|>:) = (<>)


instance Table DB a => Semigroup (NonEmptyTable a) where
  NonEmptyTable as <> NonEmptyTable bs = NonEmptyTable $
    happend (\_ _ (DB a) (DB b) -> DB (sappend a b)) as bs
