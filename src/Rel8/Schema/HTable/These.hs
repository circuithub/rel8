{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.These
  ( HTheseTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity )
import Rel8.Schema.HTable.Label ( HLabel )
import Rel8.Schema.HTable.Nullify ( HNullify )
import qualified Rel8.Schema.Kind as K
import Rel8.Type.Tag ( MaybeTag )


type HTheseTable :: K.HTable -> K.HTable -> K.HTable
data HTheseTable here there context = HTheseTable
  { hhereTag :: HLabel "hereTag" (HIdentity (Maybe MaybeTag)) context
  , hhere :: HLabel "Here" (HNullify here) context
  , hthereTag :: HLabel "thereTag" (HIdentity (Maybe MaybeTag)) context
  , hthere :: HLabel "There" (HNullify there) context
  }
  deriving stock Generic
  deriving anyclass HTable
