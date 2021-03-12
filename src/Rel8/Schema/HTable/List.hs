{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.List
  ( HListTable
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Kind.Emptiability ( Emptiability( Emptiable ) )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Vectorize ( HVectorize )


type HListTable :: HKTable -> HKTable
type HListTable = HVectorize 'Emptiable
