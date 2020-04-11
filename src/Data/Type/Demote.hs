{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Data.Type.Demote where

import Control.Applicative
import Data.Functor.Product
import Data.Functor.Sum
import Data.Type.Equality


class Demote (a :: k) where
  demote :: k

instance (Demote x, Demote y) => Demote ('Pair x y) where
  demote = Pair (demote @_ @x) (demote @_ @y)

instance Demote x => Demote ('InL x) where
  demote = InL (demote @_ @x)

instance Demote x => Demote ('Const x) where
  demote = Const (demote @_ @x)

instance Demote 'Refl where
  demote = Refl

instance Demote '() where
  demote = ()
