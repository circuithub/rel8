{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}

module Data.Indexed.Functor.Compose
  ( HCompose( HCompose )
  , getHCompose
  , I( I )
  , unI
  ) where

-- base
import Data.Functor.Compose ( Compose( Compose ), getCompose )
import Data.Kind ( Type )

-- rel8
import Data.Indexed.Functor ( HFunctor, hmap )
import Data.Indexed.Functor.Representable ( HRep, HRepresentable, hindex, htabulate )
import Data.Indexed.Functor.Traversable ( HTraversable, htraverse )


-- | Pre-apply a functor to an indexed type, thus transforming the index.
--
-- This can be used to transform the index. For example,
-- @HCompose (HIdentity Bool) Maybe@ is the same as @HIdentity (Maybe Bool)@.
newtype HCompose (h :: (x -> Type) -> Type) (f :: x -> y) (i :: y -> Type) =
  HCompose { getHCompose :: h (Compose i f) }


instance HFunctor h => HFunctor (HCompose h g) where
  hmap f =
    HCompose . hmap (Compose . f . getCompose) . getHCompose


instance HTraversable h => HTraversable (HCompose h g) where
  htraverse f =
    fmap HCompose . htraverse (fmap Compose . f . getCompose) . getHCompose


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
