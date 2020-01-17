{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Rel8.ZipLeaves where

import GHC.Exts
import Rel8.Column
import Rel8.HigherKinded


-- | Zip the "leaves" of a data structure together.
class ZipLeaves a b f g | a -> f, b -> g where
  type CanZipLeaves a b (c :: * -> Constraint) :: Constraint

  zipLeaves
    :: (CanZipLeaves a b c, Applicative m)
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> (a -> a -> m b)


instance ZipLeaves (C f a) (C g a) f g where
  type CanZipLeaves ( C f a ) ( C g a ) c = c a
  zipLeaves _proxy = id


instance HigherKinded t => ZipLeaves (t f) (t g) f g where
  type CanZipLeaves (t f) (t g) c = ZipRecord t f g c
  zipLeaves = zipRecord
