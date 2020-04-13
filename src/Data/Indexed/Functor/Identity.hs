{-# language KindSignatures #-}
{-# language TypeFamilies #-}

-- | The identity functor on indexed-types.
module Data.Indexed.Functor.Identity where

import Data.Dict ( Dict(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Constrained ( HConstrained(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
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


instance HTraversable (HIdentity a) where
  htraverse f (HIdentity x) = HIdentity <$> f x


instance HConstrained (HIdentity a) where
  type All (HIdentity a) c = c a
  hconstrained _ = HIdentity ( Compose Dict )
