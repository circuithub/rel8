{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Quintet
  ( HQuintet(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude ()

-- rel8
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K


type HQuintet :: K.HTable -> K.HTable -> K.HTable -> K.HTable -> K.HTable -> K.HTable
data HQuintet v w x y z context = HQuintet
  { hfst :: v context
  , hsnd :: w context
  , htrd :: x context
  , hfrt :: y context
  , hfft :: z context
  }
  deriving stock Generic
  deriving anyclass HTable
