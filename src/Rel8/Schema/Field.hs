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
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Schema.Structure ( Structure, Shape( Column ), Shape1 )


type Field :: K.Context -> Labels -> Necessity -> Type -> Type
type family Field labels context necessity a where
  Field Identity  _labels _necessity a = a
  Field Expr      _labels _necessity a = Expr a
  Field Insert    _labels 'Required  a = Expr a
  Field Insert    _labels 'Optional  a = Maybe (Expr a)
  Field Aggregate _labels _necessity a = Aggregate (Expr a)
  Field Structure labels  necessity  a = Shape1 'Column ('Spec labels necessity a)
  Field context   _labels _necessity a = context a
