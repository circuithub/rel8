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
import Rel8.Schema.HTable.Context ( HKTable )


type HQuintet :: HKTable -> HKTable -> HKTable -> HKTable -> HKTable -> HKTable
data HQuintet v w x y z context = HQuintet
  { hfst :: v context
  , hsnd :: w context
  , htrd :: x context
  , hfrt :: y context
  , hfft :: z context
  }
  deriving stock Generic
  deriving anyclass HTable
