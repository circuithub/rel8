{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}

module Data.Indexed.Functor.Constrained where

import Data.Dict ( Dict(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( HFunctor )
import Data.Kind ( Constraint, Type )


class HFunctor f => HConstrained (f :: (Type -> Type) -> Type) where
  type All f (c :: Type -> Constraint) :: Constraint

  hconstrained :: All f c => proxy c -> f (Compose Dict c)


instance (Applicative f, HConstrained g) => HConstrained (Compose f g) where
  type All (Compose f g) c = All g c

  hconstrained c = Compose $ pure $ hconstrained c
