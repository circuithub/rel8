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
import qualified Opaleye.Order as Opaleye


-- | An ordering expression for @a@. Primitive orderings are defined with
-- 'Rel8.asc' and 'Rel8.desc', and you can combine @Order@ via its various
-- instances.
--
-- A common pattern is to use '<>' to combine multiple orderings in sequence,
-- and 'Data.Functor.Contravariant.>$<' to select individual columns.
type Order :: Type -> Type
newtype Order a = Order (Opaleye.Order a)
  deriving newtype (Contravariant, Divisible, Decidable, Semigroup, Monoid)
