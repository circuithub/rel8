{-# language RankNTypes #-}

module Data.Indexed.Functor.Traversable where

import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( HFunctor )


class HFunctor f => HTraversable f where
  htraverse :: Applicative m => (forall x. g x -> m (h x)) -> f g -> m (f h)


instance (Traversable f, HTraversable g) => HTraversable (Compose f g) where
  htraverse f (Compose x) = Compose <$> traverse (htraverse f) x


hsequence :: (Applicative m, HTraversable f) => f (Compose m j) -> m (f j)
hsequence = htraverse getCompose
