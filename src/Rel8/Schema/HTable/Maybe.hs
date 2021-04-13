{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Maybe
  ( HMaybeTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel )
import Rel8.Schema.HTable.Nullify ( HNullify )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( MaybeTag )


type HMaybeTable :: K.HTable -> K.HTable
data HMaybeTable table context = HMaybeTable
  { htag :: HIdentity ('Spec '["isJust"] 'Required (Maybe MaybeTag)) context
  , hjust :: HLabel "Just" (HNullify table) context
  }
  deriving stock Generic
  deriving anyclass HTable
