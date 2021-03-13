{-# language DataKinds #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Schema.Value
  ( Value( NonNullableValue, NullableValue )
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability, nullabilitySing
  )


type Value :: Nullability -> Type -> Type
data Value nullability a where
  NonNullableValue :: a -> Value 'NonNullable a
  NullableValue :: Maybe a -> Value 'Nullable a


instance Semigroup a => Semigroup (Value nullability a) where
  NonNullableValue a <> NonNullableValue b = NonNullableValue (a <> b)
  NullableValue ma <> NullableValue mb = NullableValue (liftA2 (<>) ma mb)


instance (KnownNullability nullability, Monoid a) =>
  Monoid (Value nullability a)
 where
  mempty = case nullabilitySing @nullability of
    SNonNullable -> NonNullableValue mempty
    SNullable -> NullableValue (Just mempty)
