{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.HTable.Maybe
  ( HMaybeTable(..)
  , HMaybeNullifiable
  )
where

-- base
import Data.Kind ( Constraint )
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.Context.Nullify ( HConstrainTag, HNullifiable )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Type.Tag ( MaybeTag )
import Rel8.Schema.HTable.Nullify ( HNullify )


type HMaybeTable :: K.HTable -> K.HTable
data HMaybeTable table context = HMaybeTable
  { htag :: HIdentity ('Spec '["isJust"] 'Required (Maybe MaybeTag)) context
  , hjust :: HNullify table context
  }
  deriving stock Generic
  deriving anyclass HTable


type HMaybeNullifiable :: K.HContext -> Constraint
class (HNullifiable context, HConstrainTag context MaybeTag) => HMaybeNullifiable context
instance (HNullifiable context, HConstrainTag context MaybeTag) => HMaybeNullifiable context
