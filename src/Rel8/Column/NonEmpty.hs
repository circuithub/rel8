{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Rel8.Column.NonEmpty (
  HNonEmpty,
)
where

-- base
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Prelude ()

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result (Result)
import Rel8.Table.NonEmpty (NonEmptyTable)


{- | Nest a 'NonEmpty' list within a 'Rel8able'. @HNonEmpty f a@ will produce a
'NonEmptyTable' @a@ in the 'Expr' context, and a 'NonEmpty' @a@ in the
'Result' context.
-}
type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context = nonEmpty | nonEmpty -> context where
  HNonEmpty Result = NonEmpty
  HNonEmpty context = NonEmptyTable context
