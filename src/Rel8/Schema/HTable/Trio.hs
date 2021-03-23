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
import Rel8.Schema.HTable.Context ( HKTable )


type HTrio :: HKTable -> HKTable -> HKTable -> HKTable
data HTrio x y z context = HTrio
  { hfst :: x context
  , hsnd :: y context
  , htrd :: z context
  }
  deriving stock Generic
  deriving anyclass HTable
