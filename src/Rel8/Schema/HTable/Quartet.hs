{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Quartet
  ( HQuartet(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude ()

-- rel8
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K


type HQuartet :: K.HTable -> K.HTable -> K.HTable -> K.HTable -> K.HTable
data HQuartet w x y z context = HQuartet
  { hfst :: w context
  , hsnd :: x context
  , htrd :: y context
  , hfrt :: z context
  }
  deriving stock Generic
  deriving anyclass HTable
