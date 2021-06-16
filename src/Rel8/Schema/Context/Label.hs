{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Schema.Context.Label
  ( Labelable( labeler, unlabeler )
  , HLabelable( hlabeler, hunlabeler )
  )
where

-- base
import Data.Kind ( Constraint )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context ( Interpretation )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Kind ( Context, HContext )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Schema.Result ( Col( R ), Result )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )


type Labelable :: Context -> Constraint
class Interpretation context => Labelable context where
  labeler :: ()
    => Col context ('Spec labels defaulting a)
    -> Col context ('Spec (label ': labels) defaulting a)

  unlabeler :: ()
    => Col context ('Spec (label ': labels) defaulting a)
    -> Col context ('Spec labels defaulting a)


instance Labelable Result where
  labeler (R a) = R a
  unlabeler (R a) = R a


type HLabelable :: HContext -> Constraint
class HLabelable context where
  hlabeler :: ()
    => context ('Spec labels defaulting a)
    -> context ('Spec (label ': labels) defaulting a)

  hunlabeler :: ()
    => context ('Spec (label ': labels) defaulting a)
    -> context ('Spec labels defaulting a)


instance Labelable context => HLabelable (Col context) where
  hlabeler = labeler
  hunlabeler = unlabeler


instance HLabelable (Dict (ConstrainDBType constraint)) where
  hlabeler Dict = Dict
  hunlabeler Dict = Dict
