{-# language FlexibleInstances #-}

module Rel8.Unconstrained where

-- | @Unconstrained@ is a type class constraint inhabited by all Haskell types.
class Unconstrained x


instance Unconstrained x
