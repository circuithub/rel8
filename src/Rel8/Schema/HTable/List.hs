{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.List
  ( HListTable
  )
where

-- base
import Prelude ()

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable.Vectorize ( HVectorize )


type HListTable :: K.HTable -> K.HTable
type HListTable = HVectorize []
