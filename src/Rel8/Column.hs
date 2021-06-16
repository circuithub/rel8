{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Column
  ( Column, Default
  , TColumn
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Column.Field ( Field )
import Rel8.FCF ( Eval, Exp )
import Rel8.Kind.Defaulting ( Defaulting( NoDefault, HasDefault ) )
import qualified Rel8.Schema.Kind as K


-- | The @Column@ type family should be used to indicate which fields of your
-- data types are single columns in queries. This type family has special
-- support when a query is executed, allowing you to use a single data type for
-- both query data and rows decoded to Haskell.
type Column :: K.Context -> Type -> Type
type Column context a = Field context 'NoDefault a


type Default :: K.Context -> Type -> Type
type Default context a = Field context 'HasDefault a


data TColumn :: K.Context -> Type -> Exp Type
type instance Eval (TColumn f a) = Column f a
