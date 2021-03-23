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


-- | An ordering expression for @a@. Primitive orderings are defined with 'asc'
-- and 'desc', and you can combine @Order@ via its various instances.
--
-- A common pattern is to use '<>' to combine multiple orderings in sequence,
-- and '>$<' (from 'Contravariant') to select individual columns. For example,
-- to sort a @Query@ on two columns, we could do:
--
-- >>> import Data.Functor.Contravariant ((>$<))
-- >>> :{
-- select c $ orderBy (mconcat [fst >$< asc, snd >$< desc]) $ do
--   x <- values [ lit x | x <- [1..3 :: Int32 ] ]
--   y <- values [ lit x | x <- [1..3 :: Int32 ] ]
--   return (x, y)
-- :}
-- [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
type Order :: Type -> Type
newtype Order a = Order (Opaleye.Order a)
  deriving newtype (Contravariant, Divisible, Decidable, Semigroup, Monoid)
