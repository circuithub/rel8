{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.Aggregate.Fold (
  Fallback (Empty, Fallback),
  Fold (Semi, Full),
)
where

-- base
import Control.Applicative (liftA2)
import Data.Kind (Type)
import Prelude

-- semigroupoids
import Data.Functor.Apply (Apply, liftF2)


{- | 'Fold' is a kind that parameterises aggregations. Aggregations
parameterised by 'Semi' are analogous to 'Data.Semigroup.Foldable.foldMap1'
(i.e, they can only produce results on a non-empty 'Rel8.Query') whereas
aggregations parameterised by 'Full' are analagous to 'foldMap' (given a
non-empty) query, they return the identity values of the aggregation
functions.
-}
type Fold :: Type
data Fold = Semi | Full


type Fallback :: Fold -> Type -> Type
data Fallback fold a where
  Fallback :: !a -> Fallback fold a
  Empty :: Fallback 'Semi a


instance Functor (Fallback fold) where
  fmap f = \case
    Fallback a -> Fallback (f a)
    Empty -> Empty


instance Apply (Fallback fold) where
  liftF2 f (Fallback a) (Fallback b) = Fallback (f a b)
  liftF2 _ (Fallback _) Empty = Empty
  liftF2 _ Empty (Fallback _) = Empty
  liftF2 _ Empty Empty = Empty


instance Applicative (Fallback fold) where
  pure = Fallback
  liftA2 = liftF2
