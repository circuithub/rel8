{-# language TypeFamilies #-}

module Rel8.Column ( Column, C(..) ) where

import Data.Functor.Identity
import Data.Kind


type family Column ( f :: Type -> Type ) ( a :: Type ) :: Type where
  Column Identity a = a
  Column f a = f a


newtype C f x =
  C { toColumn :: Column f x }
