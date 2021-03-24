{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Field
  ( Field
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Labels ( Labels )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Schema.Context ( Insertion )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Schema.Structure ( Structure, Shape( Column ), Shape1 )


type Field :: K.Context -> Labels -> Necessity -> Type -> Type -> Type
type family Field labels context necessity db a where
  Field Identity    _labels _necessity _db a = a
  Field Expr        _labels _necessity _db a = Expr a
  Field Insertion   _labels 'Required  _db a = Expr a
  Field Insertion   _labels 'Optional  _db a = Maybe (Expr a)
  Field Aggregate   _labels _necessity _db a = Aggregate (Expr a)
  Field Structure   labels necessity   db  a = Shape1 'Column ('Spec labels necessity db a)
  Field context     _labels _necessity _db a = context a
