{-# language StandaloneKindSignatures #-}

module Rel8.Opaque
  ( Opaque
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()


type Opaque :: Type
data Opaque
