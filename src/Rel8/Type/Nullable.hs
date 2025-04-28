{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.Nullable (
  NullableOrNot (..),
) where

-- base
import Data.Kind (Type)
import Prelude


type NullableOrNot :: (Type -> Type) -> Type -> Type
data NullableOrNot decoder a where
  NonNullable :: decoder a -> NullableOrNot decoder a
  Nullable :: decoder a -> NullableOrNot decoder (Maybe a)
