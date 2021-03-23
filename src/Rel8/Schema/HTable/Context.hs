{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Context
  ( HContext, H, HKTable
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Context )


type HContext :: Type
newtype HContext = H Context


type H :: Context -> HContext
type H = 'H


type HKTable :: Type
type HKTable = HContext -> Type
