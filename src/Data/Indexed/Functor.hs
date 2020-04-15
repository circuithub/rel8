{-# language KindSignatures #-}
{-# language RankNTypes #-}

-- | Functors from indexed-types to types.
module Data.Indexed.Functor ( HFunctor(..) ) where

import Data.Functor.Compose ( Compose(..) )
import Data.Kind ( Type )


class HFunctor (f :: (Type -> Type) -> Type) where
  hmap :: (forall x. g x -> h x) -> f g -> f h


instance (Functor f, HFunctor g) => HFunctor (Compose f g) where
  hmap f (Compose x) =
    Compose (fmap (hmap f) x)
