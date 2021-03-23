{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.NonEmpty
  ( HNonEmptyTable
  )
where

-- base
import Data.List.NonEmpty ( NonEmpty )
import Prelude ()

-- rel8
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Vectorize ( HVectorize )


type HNonEmptyTable :: HKTable -> HKTable
type HNonEmptyTable = HVectorize NonEmpty
