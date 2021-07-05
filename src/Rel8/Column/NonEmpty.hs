{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Column.NonEmpty
  ( HNonEmpty
  )
where

-- base
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )
import Rel8.Table.NonEmpty ( NonEmptyTable )


-- | Nest a 'NonEmpty' list within a 'Rel8able'. @HNonEmpty f a@ will produce a
-- 'NonEmptyTable' @a@ in the 'Expr' context, and a 'NonEmpty' @a@ in the
-- 'Result' context.
type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context = nonEmpty | nonEmpty -> context where
  HNonEmpty Aggregate = NonEmptyTable Aggregate
  HNonEmpty Expr = NonEmptyTable Expr
  HNonEmpty Name = NonEmptyTable Name
  HNonEmpty Result = NonEmpty
