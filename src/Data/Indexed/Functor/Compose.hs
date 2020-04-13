{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Data.Indexed.Functor.Compose where

import Data.Kind ( Constraint, Type )
import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )


newtype HCompose (h :: (Type -> Type) -> Type) (f :: Type -> Type) (i :: Type -> Type) =
  HCompose (h (Compose i f))


instance HFunctor h => HFunctor (HCompose h g) where
  hmap f (HCompose x) = HCompose $ hmap (\(Compose y) -> Compose (f y)) x


class g (f x) => CCompose (g :: j -> Constraint) (f :: k -> j) (x :: k) where


class (forall x. c x => c (g x)) => Imply (c :: Type -> Constraint) (g :: Type -> Type)
instance (forall x. c x => c (g x)) => Imply (c :: Type -> Constraint) (g :: Type -> Type)


instance HTraversable h => HTraversable (HCompose h g) where
  htraverse f (HCompose x) = HCompose <$> htraverse (\(Compose y) -> Compose <$> f y) x


data I (h :: (Type -> Type) -> Type) (g :: Type -> Type) (x :: Type) where
  I :: HRep h y -> I h g (g y)


instance HRepresentable h => HRepresentable (HCompose h g) where
  type HRep (HCompose h g) = I h g
  hindex (HCompose a) (I i) = getCompose $ hindex a i
  htabulate f = HCompose $ htabulate \i -> Compose (f (I i))
