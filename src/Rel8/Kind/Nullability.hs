{-# language DataKinds #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability ( SNullable, SNonNullable )
  , KnownNullability( nullabilitySing )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()


type Nullability :: Type
data Nullability = Nullable | NonNullable


type SNullability :: Nullability -> Type
data SNullability nullability where
  SNullable :: SNullability 'Nullable
  SNonNullable :: SNullability 'NonNullable


type KnownNullability :: Nullability -> Constraint
class KnownNullability nullability where
  nullabilitySing :: SNullability nullability


instance KnownNullability 'Nullable where
  nullabilitySing = SNullable


instance KnownNullability 'NonNullable where
  nullabilitySing = SNonNullable
