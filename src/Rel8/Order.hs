{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Order
  ( Order(..)
  )
where

-- base
import Data.Functor.Contravariant ( Contravariant )
import Data.Kind ( Type )
import Prelude

-- contravariant
import Data.Functor.Contravariant.Divisible ( Decidable, Divisible )

-- opaleye
import qualified Opaleye.Internal.Order as Opaleye


type Order :: Type -> Type
newtype Order a = Order (Opaleye.Order a)
  deriving newtype (Contravariant, Divisible, Decidable, Semigroup, Monoid)
