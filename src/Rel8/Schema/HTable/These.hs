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
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity )
import Rel8.Schema.HTable.Nullify ( HNullify )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( MaybeTag )


type HTheseTable :: K.HTable -> K.HTable -> K.HTable
data HTheseTable here there context = HTheseTable
  { hhereTag :: HIdentity ('Spec '["hasHere"] 'Required (Maybe MaybeTag)) context
  , hhere :: HNullify here context
  , hthereTag :: HIdentity ('Spec '["hasThere"] 'Required (Maybe MaybeTag)) context
  , hthere :: HNullify there context
  }
  deriving stock Generic
  deriving anyclass HTable
