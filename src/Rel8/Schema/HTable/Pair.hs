{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Pair
  ( HPair(..)
  )
where

-- base
import Prelude ()

-- rel8
import qualified Rel8.Schema.Kind as K


type HPair :: K.HTable -> K.HTable -> K.HTable
data HPair fst snd context = HPair
  { hfst :: fst context
  , hsnd :: snd context
  }
