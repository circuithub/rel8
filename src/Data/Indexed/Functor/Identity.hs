{-# language KindSignatures #-}
{-# language TypeFamilies #-}

-- | The identity functor on indexed-types.
module Data.Indexed.Functor.Identity where

import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)(..) )


newtype HIdentity (a :: Type) (g :: Type -> Type) =
  HIdentity { unHIdentity :: g a }


instance HFunctor (HIdentity a) where
  hmap f (HIdentity x) = HIdentity (f x)


instance HRepresentable (HIdentity a) where
  type HRep (HIdentity a) = (:~:) a
  hindex (HIdentity x) Refl = x
  htabulate f = HIdentity (f Refl)
