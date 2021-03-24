{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
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
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Schema.Context
  ( Interpretation, Col, Col'(..), IsSpecialContext
  , Insertion
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )


type Labelable :: K.Context -> Constraint
class Interpretation context => Labelable context where
  labeler :: ()
    => Col context ('Spec labels necessity db a)
    -> Col context ('Spec (label ': labels) necessity db a)

  unlabeler :: ()
    => Col context ('Spec (label ': labels) necessity db a)
    -> Col context ('Spec labels necessity db a)


instance Labelable Aggregate where
  labeler (Aggregation aggregate) = Aggregation aggregate
  unlabeler (Aggregation aggregate) = Aggregation aggregate


instance Labelable Expr where
  labeler (DB a) = DB a
  unlabeler (DB a) = DB a


instance Labelable Insertion where
  labeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma

  unlabeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma


instance Labelable Identity where
  labeler (Result a) = Result a
  unlabeler (Result a) = Result a


instance {-# OVERLAPPABLE #-} IsSpecialContext context ~ 'False =>
  Labelable context
 where
  labeler (Col a) = Col a
  unlabeler (Col a) = Col a


type HLabelable :: K.HContext -> Constraint
class HLabelable context where
  hlabeler :: ()
    => context ('Spec labels necessity db a)
    -> context ('Spec (label ': labels) necessity db a)

  hunlabeler :: ()
    => context ('Spec (label ': labels) necessity db a)
    -> context ('Spec labels necessity db a)


instance (Labelable context, x ~ IsSpecialContext context) =>
  HLabelable (Col' x context)
 where
  hlabeler = labeler
  hunlabeler = hunlabeler
