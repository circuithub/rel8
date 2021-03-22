{-# language TypeFamilies #-}

module Rel8.Nullify ( Nullify ) where

import Data.Kind ( Type )


type family Nullify (a :: Type) :: Type where
  Nullify (Maybe a) = Maybe a
  Nullify a         = Maybe a
