{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Column
  ( Column, Default
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Column.Field ( Field )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import qualified Rel8.Schema.Kind as K


-- | The @Column@ type family should be used to indicate which fields of your
-- data types are single columns in queries. This type family has special
-- support when a query is executed, allowing you to use a single data type for
-- both query data and rows decoded to Haskell.
type Column :: K.Context -> Type -> Type
type Column context a = Field context 'Required a


type Default :: K.Context -> Type -> Type
type Default context a = Field context 'Optional a
