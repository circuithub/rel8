{-# language BlockArguments #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Data.Indexed.Functor.Compose ( HCompose(..), I(..) ) where

import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Kind ( Type )


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
