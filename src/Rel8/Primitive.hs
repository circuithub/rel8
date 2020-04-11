{-# language KindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Primitive where

import Data.Kind ( Type )
import Data.Functor.Identity ( Identity )


class PrimLike (f :: Type -> Type) where
  data Primitive a f :: Type


instance PrimLike Identity where
  newtype Primitive a Identity = PrimIdentity a
