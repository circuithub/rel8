{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.Schema.HTable.List (
  HListTable,
)
where

-- base
import Prelude ()

-- rel8
import Rel8.Schema.HTable.Vectorize (HVectorize)
import qualified Rel8.Schema.Kind as K


type HListTable :: K.HTable -> K.HTable
type HListTable = HVectorize []
