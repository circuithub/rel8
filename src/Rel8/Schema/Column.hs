{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Column
  ( Column, Default
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Schema.Field ( Field )
import qualified Rel8.Schema.Kind as K


type Default :: Type -> Type
data Default a


type GetNecessity :: Type -> Necessity
type family GetNecessity a where
  GetNecessity (Default _) = 'Optional
  GetNecessity _ = 'Required


type UnwrapDefault :: Type -> Type
type family UnwrapDefault a where
  UnwrapDefault (Default a) = a
  UnwrapDefault a = a


-- | The @Column@ type family should be used to indicate which fields of your
-- data types are single columns in queries. This type family has special
-- support when a query is executed, allowing you to use a single data type for
-- both query data and rows decoded to Haskell.
type Column :: K.Context -> Type -> Type
type Column context a =
  Field context (GetNecessity a) (UnwrapDefault a)
