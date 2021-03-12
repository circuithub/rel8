{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Table.Alternative
  ( AltTable ( (<|>:) )
  , AlternativeTable ( emptyTable )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Schema.Context ( DB )
import Rel8.Table ( Table, Context )


type AltTable :: (Type -> Type) -> Constraint
class AltTable f where
  (<|>:) :: (Context a ~ DB, Table a) => f a -> f a -> f a
  infixl 3 <|>:


type AlternativeTable :: (Type -> Type) -> Constraint
class AltTable f => AlternativeTable f where
  emptyTable :: (Context a ~ DB, Table a) => f a
