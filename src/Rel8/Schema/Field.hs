{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Field
  ( Field
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Labels ( Labels )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Schema.Context
  ( Aggregation, DB, Insertion, Result
  , IsSpecialContext
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )
import Rel8.Schema.Structure ( Structure, Shape( Column ), Shape1 )


type IField :: Bool -> Context -> Labels -> Necessity -> Type -> Type -> Type
type family IField isSpecialContext labels context necessity db a where
  IField 'False context     labels  necessity  db  a = context ('Spec labels necessity db a)
  IField 'True  Result      _labels _necessity _db a = a
  IField 'True  DB          _labels _necessity _db a = Expr a
  IField 'True  Insertion   _labels 'Required  _db a = Expr a
  IField 'True  Insertion   _labels 'Optional  _db a = Maybe (Expr a)
  IField 'True  Aggregation _labels _necessity _db a = Aggregate (Expr a)
  IField 'True  Structure   labels necessity   db  a = Shape1 'Column ('Spec labels necessity db a)


type Field :: Context -> Labels -> Necessity -> Type -> Type -> Type
type Field context labels necessity db a =
  IField (IsSpecialContext context) context labels necessity db a
