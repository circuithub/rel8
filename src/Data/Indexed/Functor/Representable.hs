{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

-- | Representable functors from indexed-types to types.
module Data.Indexed.Functor.Representable where

import Control.Applicative ( Const(..) )
import Data.Functor.Rep ( Representable(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Kind ( Type )


class HFunctor f => HRepresentable (f :: (Type -> Type) -> Type) where
  type HRep f :: Type -> Type
  hindex :: f x -> (forall y. HRep f y -> x y)
  htabulate :: (forall y. HRep f y -> x y) -> f x


instance (Representable f, HRepresentable g) => HRepresentable (Compose f g) where
  type HRep (Compose f g) = Product (Const (Rep f)) (HRep g)

  hindex (Compose x) (Pair (Const i) j) = hindex (index x i) j
  htabulate f = Compose (tabulate (\i -> htabulate (f . Pair (Const i))))
