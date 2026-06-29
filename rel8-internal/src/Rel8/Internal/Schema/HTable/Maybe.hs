{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Internal.Schema.HTable.Maybe
  ( HMaybeTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Internal.Schema.HTable ( HTable )
import Rel8.Internal.Schema.HTable.Identity ( HIdentity )
import Rel8.Internal.Schema.HTable.Label ( HLabel )
import Rel8.Internal.Schema.HTable.Nullify ( HNullify )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Type.Tag ( MaybeTag )


type HMaybeTable :: K.HTable -> K.HTable
data HMaybeTable table context = HMaybeTable
  { htag :: HLabel "isJust" (HIdentity (Maybe MaybeTag)) context
  , hjust :: HLabel "Just" (HNullify table) context
  }
  deriving stock Generic
  deriving anyclass HTable
