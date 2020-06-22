{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rel8.Internal.Orphans
  (
  )
where

-- base
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

-- opaleye
import Opaleye.Internal.Lateral ( lateral )
import Opaleye.QueryArr

-- profunctors
import Data.Profunctor.Choice
import Data.Profunctor.Strong
import Data.Profunctor.Traversing


instance Choice QueryArr where
  right' = right


instance Strong QueryArr where
  second' = second


instance Traversing QueryArr where
  wander = wanderA


instance ArrowApply QueryArr where
  app = lateral (\(f, i) -> f <<< pure i)


instance Monad (QueryArr a) where
  return = pure
  as >>= f = lateral (\(i, a) -> f a <<< pure i) <<< (id &&& as)


------------------------------------------------------------------------------
-- Everything that follows is stolen from:
-- https://github.com/ekmett/profunctors/pull/40/files


infixl 4 :<*>

-- A list of values that have been traversed over so far. d is the input type;
-- e is the planned output.
data TList d e a where
  TNil :: TList d e ()
  (:<*>) :: d -> TList d e u -> TList d e (e, u)

-- Trav is a Church-encoded free applicative, which is used to make traversing
-- and assembling a TList faster by left-associating and bringing all the
-- fmaps to the top. See https://www.eyrie.org/~zednenem/2013/06/freeapp-2 for
-- details.
newtype Trav d e a = Trav (forall u y z. (forall x. (x -> y) -> TList d e x -> z) -> (u -> a -> y) -> TList d e u -> z)

instance Functor (Trav d e) where
  {-# INLINE fmap #-}
  fmap f (Trav m) = Trav $ \k s -> m k (\u -> s u . f)

  {-# INLINE (<$) #-}
  a <$ Trav m = Trav $ \k s -> m k (\u -> const $ s u a)

instance Applicative (Trav d e) where
  {-# INLINE pure #-}
  pure a = Trav $ \k s -> k (flip s a)

  {-# INLINE (<*>) #-}
  Trav mf <*> Trav ma = Trav $ \k s -> ma (mf k) (\u a g -> s u (g a))

-- Coyoneda encoding of a Functor.
data Coyo f a where
  Coyo :: (u -> a) -> f u -> Coyo f a

-- Lift a d into an appropriate Trav with an unknown return type.
{-# INLINE tLift #-}
tLift :: d -> Trav d e e
tLift d = Trav $ \k s p -> k (\ (a, u) -> s u a) (d :<*> p)

-- Convert the Trav into an actual list.
{-# INLINE runTrav #-}
runTrav :: Trav d e a -> Coyo (TList d e) a
runTrav (Trav m) = m Coyo (const id) TNil

-- Split a Coyoneda-encoded TList into something an ArrowChoice can traverse.
{-# INLINE unTList #-}
unTList :: Coyo (TList d e) a -> Either a (d, Coyo (TList d e) (e -> a))
unTList (Coyo f TNil) = Left (f ())
unTList (Coyo f (d :<*> t)) = Right (d, Coyo (\u e -> f (e, u)) t)

{-# INLINE wanderA #-}
wanderA :: forall p a b s t. (ArrowChoice p)
  => (forall f. (Applicative f) => (a -> f b) -> s -> f t)
  -> p a b -> p s t
wanderA tr p = go . arr (runTrav . tr tLift) where
  go :: forall u. p (Coyo (TList a b) u) u
  go = (id ||| arr (uncurry $ flip id) . (p *** go)) . arr unTList
