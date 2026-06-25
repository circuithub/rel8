{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Internal.Schema.HTable.Product
  ( HProduct(..)
  )
where

-- base
import Prelude ()

-- rel8
import qualified Rel8.Internal.Schema.Kind as K


type HProduct :: K.HTable -> K.HTable -> K.HTable
data HProduct a b context = HProduct (a context) (b context)
