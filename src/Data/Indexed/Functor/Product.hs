{-# language KindSignatures #-}
{-# language TypeFamilies #-}

-- | The product of two functors on indexed-types.
module Data.Indexed.Functor.Product where

import Data.Functor.Sum ( Sum(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Kind ( Type )


data HProduct (f :: (Type -> Type) -> Type) (g :: (Type -> Type) -> Type) (h :: Type -> Type) =
  HProduct { hfst :: f h, hsnd :: g h }


instance (HFunctor f, HFunctor g) => HFunctor (HProduct f g) where
  hmap f (HProduct x y) = HProduct (hmap f x) (hmap f y)


instance (HRepresentable f, HRepresentable g) => HRepresentable (HProduct f g) where
  type HRep (HProduct f g) = Sum (HRep f) (HRep g)

  hindex (HProduct x _y) (InL rep) = hindex x rep
  hindex (HProduct _x y) (InR rep) = hindex y rep

  htabulate f = HProduct (htabulate (f . InL)) (htabulate (f . InR))


instance (HTraversable f, HTraversable g) => HTraversable (HProduct f g) where
  htraverse f (HProduct x y) = HProduct <$> htraverse f x <*> htraverse f y
