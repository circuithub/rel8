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
import Control.Applicative ( Const(..) )
import Data.Functor.Compose ( Compose )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context ( Interpretation, Col(..) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Kind ( Context, HContext )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )


type Labelable :: Context -> Constraint
class Interpretation context => Labelable context where
  labeler :: ()
    => Col context ('Spec labels necessity a)
    -> Col context ('Spec (label ': labels) necessity a)

  unlabeler :: ()
    => Col context ('Spec (label ': labels) necessity a)
    -> Col context ('Spec labels necessity a)


instance Labelable Identity where
  labeler (Result a) = Result a
  unlabeler (Result a) = Result a


instance Labelable (Const x) where
  labeler (K a) = K a
  unlabeler (K a) = K a


type HLabelable :: HContext -> Constraint
class HLabelable context where
  hlabeler :: ()
    => context ('Spec labels necessity a)
    -> context ('Spec (label ': labels) necessity a)

  hunlabeler :: ()
    => context ('Spec (label ': labels) necessity a)
    -> context ('Spec labels necessity a)


instance Labelable context => HLabelable (Col context) where
  hlabeler = labeler
  hunlabeler = unlabeler


instance HLabelable (Dict (ConstrainDBType constraint)) where
  hlabeler Dict = Dict
  hunlabeler Dict = Dict
