{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Data.Indexed.Functor.Compose where

import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Kind ( Constraint, Type )


-- | Pre-apply a functor to an indexed type, thus transforming the index.
--
-- This can be used to transform the index. For example,
-- @HCompose (HIdentity Bool) Maybe@ is the same as @HIdentity (Maybe Bool)@.
newtype HCompose (h :: (x -> Type) -> Type) (f :: x -> y) (i :: y -> Type) =
  HCompose (h (Compose i f))


instance HFunctor h => HFunctor (HCompose h g) where
  hmap f (HCompose x) =
    HCompose $ hmap (Compose . f . getCompose) x


class (forall x. c x => c (g x)) => Imply (c :: Type -> Constraint) (g :: Type -> Type)


instance (forall x. c x => c (g x)) => Imply (c :: Type -> Constraint) (g :: Type -> Type)


instance HTraversable h => HTraversable (HCompose h g) where
  htraverse f (HCompose x) =
    HCompose <$> htraverse (\(Compose y) -> Compose <$> f y) x


-- | A witness that the index has been transformed by a functor.
--
-- This type is used to form the representation of @HCompose h g@. See the
-- @HRepresentable@ instance for more.
data I (h :: (Type -> Type) -> Type) (g :: Type -> Type) (x :: Type) where
  I :: { unI :: HRep h y } -> I h g (g y)


instance HRepresentable h => HRepresentable (HCompose h g) where
  type HRep (HCompose h g) =
    I h g

  hindex (HCompose a) (I i) =
    getCompose $ hindex a i

  htabulate f =
    HCompose $ htabulate $ Compose . f . I
