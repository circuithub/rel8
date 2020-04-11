{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Rel8.Primitive where

import Data.Kind ( Type )
import GHC.Exts ( Constraint )
import Data.Functor.Identity ( Identity )


class PrimLike (f :: Type -> Type) where
  data Primitive a f :: Type


instance PrimLike Identity where
  newtype Primitive a Identity = PrimIdentity a


class TraversePrimitives (f :: (Type -> Type) -> Type) where
  type All (c :: Type -> Constraint) f :: Constraint

  traversePrimitives :: All c f => (forall x. c x => Primitive x g -> m (Primitive x h)) -> f g -> m (f h)
