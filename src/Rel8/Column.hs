{-# language TypeFamilies #-}

module Rel8.Column ( Column, C(..) ) where

import Data.Functor.Identity
import Data.Kind


-- | The @Column@ type family should be used to indicate which fields of your
-- data types are single columns in queries. This type family has special
-- support when a query is executed, allowing you to use a single data type for
-- both query data and rows decoded to Haskell.
type family Column ( f :: Type -> Type ) ( a :: Type ) :: Type where
  Column Identity a = a
  Column f a = f a


newtype C f x =
  C { toColumn :: Column f x }
