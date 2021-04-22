{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Order
  ( Order(..)
  , toOrderExprs
  )
where

-- base
import Data.Functor.Contravariant ( Contravariant )
import Data.Kind ( Type )
import Prelude

-- contravariant
import Data.Functor.Contravariant.Divisible ( Decidable, Divisible )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Order as Opaleye


-- | An ordering expression for @a@. Primitive orderings are defined with
-- 'Rel8.asc' and 'Rel8.desc', and you can combine @Order@ via its various
-- instances.
--
-- A common pattern is to use '<>' to combine multiple orderings in sequence,
-- and '>$<' (from 'Contravariant') to select individual columns.
type Order :: Type -> Type
newtype Order a = Order (Opaleye.Order a)
  deriving newtype (Contravariant, Divisible, Decidable, Semigroup, Monoid)


toOrderExprs :: Order a -> a -> [Opaleye.OrderExpr]
toOrderExprs (Order (Opaleye.Order order)) a =
  uncurry Opaleye.OrderExpr <$> order a
