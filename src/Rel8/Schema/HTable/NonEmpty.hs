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
import Rel8.Schema.HTable.Vectorize ( HVectorize )
import qualified Rel8.Schema.Kind as K


type HNonEmptyTable :: K.HTable -> K.HTable
type HNonEmptyTable = HVectorize NonEmpty
