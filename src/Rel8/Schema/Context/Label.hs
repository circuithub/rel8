{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Schema.Context.Label
  ( Labelable( labeler, unlabeler )
  , HLabelable( hlabeler, hunlabeler )
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context ( Interpretation, Col(..) )
import Rel8.Schema.Kind ( Context, HContext )
import Rel8.Schema.Spec ( Spec( Spec ) )


type Labelable :: Context -> Constraint
class Interpretation context => Labelable context where
  labeler :: ()
    => Col context ('Spec labels necessity db a)
    -> Col context ('Spec (label ': labels) necessity db a)

  unlabeler :: ()
    => Col context ('Spec (label ': labels) necessity db a)
    -> Col context ('Spec labels necessity db a)


instance Labelable Identity where
  labeler (Result a) = Result a
  unlabeler (Result a) = Result a


type HLabelable :: HContext -> Constraint
class HLabelable context where
  hlabeler :: ()
    => context ('Spec labels necessity db a)
    -> context ('Spec (label ': labels) necessity db a)

  hunlabeler :: ()
    => context ('Spec (label ': labels) necessity db a)
    -> context ('Spec labels necessity db a)


instance Labelable context => HLabelable (Col context) where
  hlabeler = labeler
  hunlabeler = unlabeler
