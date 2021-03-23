{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Pair
  ( HPair(..)
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Schema.HTable.Context ( HKTable )


type HPair :: HKTable -> HKTable -> HKTable
data HPair fst snd context = HPair
  { hfst :: fst context
  , hsnd :: snd context
  }
