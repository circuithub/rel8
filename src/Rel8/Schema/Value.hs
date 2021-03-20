{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Schema.Value
  ( Value( NonNullableValue, NullableValue )
  , FromValue, ToValue
  , fromValue, toValue
  , GetNullability, GetValue
  , nullableMaybeLemma
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

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
deriving stock instance Show a => Show (Value nullability a)


instance Semigroup a => Semigroup (Value nullability a) where
  NonNullableValue a <> NonNullableValue b = NonNullableValue (a <> b)
  NullableValue ma <> NullableValue mb = NullableValue (liftA2 (<>) ma mb)


instance (KnownNullability nullability, Monoid a) =>
  Monoid (Value nullability a)
 where
  mempty = case nullabilitySing @nullability of
    SNonNullable -> NonNullableValue mempty
    SNullable -> NullableValue (Just mempty)


type FromValue :: Nullability -> Type -> Type
type family FromValue nullability a where
  FromValue 'Nullable a = Maybe a
  FromValue 'NonNullable a = a


type ToValue :: Type -> (Nullability, Type)
type ToValue a = '(GetNullability a, GetValue (GetNullability a) a)


fromValue :: a ~ FromValue nullability value => Value nullability value -> a
fromValue = \case
  NonNullableValue a -> a
  NullableValue a -> a


toValue :: forall a nullability value.
  ( KnownNullability nullability
  , '(nullability, value) ~ ToValue a
  )
  => a -> Value nullability value
toValue a = case nullabilitySing @nullability of
  SNullable -> case nullableMaybeLemma @a of
    Refl -> NullableValue a
  SNonNullable -> NonNullableValue a


type GetNullability :: Type -> Nullability
type family GetNullability a where
  GetNullability (Maybe _) = 'Nullable
  GetNullability _ = 'NonNullable


type GetValue :: Nullability -> Type -> Type
type family GetValue nullability a where
  GetValue 'Nullable (Maybe a) = a
  GetValue 'NonNullable a = a


nullableMaybeLemma :: GetNullability a ~ 'Nullable
  => a :~: Maybe (GetValue 'Nullable a)
nullableMaybeLemma = unsafeCoerce Refl
