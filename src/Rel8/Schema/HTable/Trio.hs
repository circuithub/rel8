{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Trio
  ( HTrio(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude ()

-- rel8
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K


type HTrio :: K.HTable -> K.HTable -> K.HTable -> K.HTable
data HTrio x y z context = HTrio
  { hfst :: x context
  , hsnd :: y context
  , htrd :: z context
  }
  deriving stock Generic
  deriving anyclass HTable
