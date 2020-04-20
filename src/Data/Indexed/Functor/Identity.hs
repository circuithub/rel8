{-# language TypeFamilies #-}

-- | The identity functor on indexed-types.

module Data.Indexed.Functor.Identity ( HIdentity( HIdentity ), unHIdentity ) where

-- base
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )

-- rel8
import Data.Indexed.Functor ( HFunctor, hmap )
import Data.Indexed.Functor.Representable ( HRep, HRepresentable, hindex, htabulate )
import Data.Indexed.Functor.Traversable ( HTraversable, htraverse )


-- | The identity indexed-type functor maps the supplied functor to the functor
-- applied at a given index.
--
-- @HIdentity Bool Identity@ ~ @Identity Bool@
newtype HIdentity (a :: Type) (g :: Type -> Type) =
  HIdentity { unHIdentity :: g a }


instance HFunctor (HIdentity a) where
  hmap f =
    HIdentity . f . unHIdentity


instance HRepresentable (HIdentity a) where
  type HRep (HIdentity a) =
    (:~:) a

  hindex (HIdentity x) Refl =
    x

  htabulate f =
    HIdentity (f Refl)


instance HTraversable (HIdentity a) where
  htraverse f =
    fmap HIdentity . f . unHIdentity
