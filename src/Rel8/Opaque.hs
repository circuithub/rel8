{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Opaque
  ( Opaque
  , Opaque1
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()


type Opaque :: Type
data Opaque


type Opaque1 :: k -> Type -> Type
data Opaque1 a x
