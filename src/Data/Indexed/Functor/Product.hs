{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

-- | The product of two functors on indexed-types.

module Data.Indexed.Functor.Product ( HProduct(..) ) where

import Control.Applicative ( liftA2 )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Functor.Sum ( Sum(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Singletons.Prelude ( If )
import GHC.Records.Compat ( HasField(..) )
import GHC.TypeLits ( Symbol )


-- | The product of two functors on indexed-type is itself an functor on
-- indexed-types.
data HProduct (f :: (Type -> Type) -> Type) (g :: (Type -> Type) -> Type) (h :: Type -> Type) =
  HProduct { hfst :: f h, hsnd :: g h }


instance (HFunctor f, HFunctor g) => HFunctor (HProduct f g) where
  hmap f =
    HProduct <$> hmap f . hfst <*> hmap f . hsnd


instance (HRepresentable f, HRepresentable g) => HRepresentable (HProduct f g) where
  type HRep (HProduct f g) =
    Sum (HRep f) (HRep g)

  hindex (HProduct x y) = \case
    InL rep -> hindex x rep
    InR rep -> hindex y rep

  htabulate f =
    HProduct (htabulate (f . InL)) (htabulate (f . InR))


instance (HTraversable f, HTraversable g) => HTraversable (HProduct f g) where
  htraverse f =
    liftA2 HProduct <$> htraverse f . hfst <*> htraverse f . hsnd


type family HasName (name :: Symbol) f :: Bool where
  HasName name (Compose (FieldName name) y) = 'True
  HasName name _ = 'False


type family WhichSide (name :: Symbol) f g r :: Side where
  WhichSide name f g r = If (HasName name f) 'L 'R


data Side = L | R


class HProductHasField f g r (side :: Side) (name :: Symbol) i | name f g i -> r where
  hproductHasField :: Proxy side -> Proxy name -> HProduct f g i -> (r -> HProduct f g i, r)


instance HasField name (f i) r => HProductHasField f g r 'L name i where
  hproductHasField Proxy Proxy (HProduct x y) = (setter, getter) where
    setter r = HProduct (fst (hasField @name x) r) y
    getter = snd (hasField @name x)


instance HasField name (g i) r => HProductHasField f g r 'R name i where
  hproductHasField Proxy Proxy (HProduct x y) = (setter, getter) where
    setter r = HProduct x (fst (hasField @name y) r)
    getter = snd (hasField @name y)


instance HProductHasField f g r (WhichSide name f g r) name i => HasField name (HProduct f g i) r where
  hasField =
    hproductHasField (Proxy @(WhichSide name f g r)) (Proxy @name)
