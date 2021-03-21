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
import Prelude ()

-- rel8
import Rel8.Kind.Blueprint ( Blueprint( Scalar ) )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Kind.Nullability ( Nullability( Nullable ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Nullify ( HNullify )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( MaybeTag )


type HMaybeTable :: HKTable -> HKTable
data HMaybeTable table context = HMaybeTable
  { htag :: HIdentity ('Spec '["isJust"] 'Required 'Nullable ('Scalar MaybeTag)) context
  , hjust :: HNullify table context
  }
  deriving stock Generic
  deriving anyclass HTable
