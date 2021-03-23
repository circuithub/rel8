{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Context ( HKTable ) where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Spec )


type HKTable :: Type
type HKTable = (Spec -> Type) -> Type
