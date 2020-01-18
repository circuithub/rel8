{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.ZipLeaves where

import Data.Monoid
import GHC.Exts
import Rel8.Column
import Rel8.HigherKinded
import Rel8.Top


-- | Zip the "leaves" of a data structure together.
class CanZipLeaves a b Top => ZipLeaves a b f g | a -> f, b -> g where
  type CanZipLeaves a b (c :: * -> Constraint) :: Constraint

  zipLeaves
    :: (CanZipLeaves a b c, Applicative m)
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> (a -> a -> m b)


instance ZipLeaves (C f a) (C g a) f g where
  type CanZipLeaves ( C f a ) ( C g a ) c = c a
  zipLeaves _proxy = id


instance ( ZipRecord t f g Top, HigherKinded t ) => ZipLeaves (t f) (t g) f g where
  type CanZipLeaves (t f) (t g) c = ZipRecord t f g c
  zipLeaves = zipRecord


instance ( ZipLeaves a c f g, ZipLeaves b d f g ) => ZipLeaves ( a, b ) ( c, d ) f g where
  type CanZipLeaves ( a, b ) ( c, d ) constraint =
    ( CanZipLeaves a c constraint, CanZipLeaves b d constraint )

  zipLeaves proxy f ( a, b ) ( c, d ) =
    (,) <$> zipLeaves proxy f a c <*> zipLeaves proxy f b d


instance ( ZipLeaves a1 a2 f g, ZipLeaves b1 b2 f g, ZipLeaves c1 c2 f g ) => ZipLeaves ( a1, b1, c1 ) ( a2, b2, c2 ) f g where
  type CanZipLeaves ( a1, b1, c1 ) ( a2, b2, c2 ) constraint =
    ( CanZipLeaves a1 a2 constraint
    , CanZipLeaves b1 b2 constraint
    , CanZipLeaves c1 c2 constraint
    )

  zipLeaves proxy f ( a1, b1, c1 ) ( a2, b2, c2 ) =
    (,,)
      <$> zipLeaves proxy f a1 a2
      <*> zipLeaves proxy f b1 b2
      <*> zipLeaves proxy f c1 c2


instance ZipLeaves a b f g => ZipLeaves ( Sum a ) ( Sum b ) f g where
  type CanZipLeaves ( Sum a ) ( Sum b ) c =
    CanZipLeaves a b c

  zipLeaves proxy f ( Sum a ) ( Sum b ) =
    Sum <$> zipLeaves proxy f a b
