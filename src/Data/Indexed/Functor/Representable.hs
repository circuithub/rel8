{-# language BlockArguments #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

-- | Representable functors from indexed-types to types.
module Data.Indexed.Functor.Representable where

import Control.Applicative ( Const(..), liftA2 )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )
import Data.Functor.Rep ( Representable(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Kind ( Type )


-- | The class of representable functors of type-indexed types.
--
-- Categorically, a functor-indexed functor is a functor from the category of
-- indexed-types to Set. For such a functor to be representable, it is
-- isomorphic to Hom(A, -), for some indexed-type A.
--
-- In this encoding, @A@ is captured as @HRep f@, and the two parts of the
-- isomorphism are witnessed by @hindex@ and @htabulate@.
class HFunctor f => HRepresentable (f :: (Type -> Type) -> Type) where
  type HRep f :: Type -> Type

  hindex :: f x -> (forall y. HRep f y -> x y)

  htabulate :: (forall y. HRep f y -> x y) -> f x


instance (Representable f, HRepresentable g) => HRepresentable (Compose f g) where
  type HRep (Compose f g) =
    Product (Const (Rep f)) (HRep g)

  hindex (Compose x) (Pair (Const i) j) =
    hindex (index x i) j

  htabulate f =
    Compose (tabulate (\i -> htabulate (f . Pair (Const i))))


hzipWith
  :: HRepresentable f
  => (forall x. i x -> j x -> k x) -> f i -> f j -> f k
hzipWith f x y =
  htabulate $ liftA2 f (hindex x) (hindex y)
