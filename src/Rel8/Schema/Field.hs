{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Field
  ( Field
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate )
import Rel8.Kind.Blueprint ( Blueprint, ToDBType, ToType )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Kind.Nullability ( Nullability ( Nullable, NonNullable ) )
import Rel8.Schema.Context
  ( Aggregation, DB, Insert, Result
  , IsSpecialContext
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )
import Rel8.Schema.Structure ( Structure, Shape( Column ), Shape1 )


type IField :: Bool -> Context -> Necessity -> Nullability -> Blueprint -> Type
type family IField isSpecialContext context necessity nullability blueprint where
  IField 'False context     necessity  nullability  blueprint = context ('Spec necessity nullability blueprint)
  IField 'True  Result      _necessity 'NonNullable blueprint = ToType blueprint
  IField 'True  Result      _necessity 'Nullable    blueprint = Maybe (ToType blueprint)
  IField 'True  DB          _necessity nullability  blueprint = Expr nullability (ToDBType blueprint)
  IField 'True  Insert      'Required  nullability  blueprint = Expr nullability (ToDBType blueprint)
  IField 'True  Insert      'Optional  nullability  blueprint = Maybe (Expr nullability (ToDBType blueprint))
  IField 'True  Aggregation _necessity nullability  blueprint = Aggregate nullability (ToDBType blueprint)
  IField 'True  Structure   necessity  nullability  blueprint = Shape1 'Column ('Spec necessity nullability blueprint)


type Field :: Context -> Necessity -> Nullability -> Blueprint -> Type
type Field context necessity nullability blueprint =
  IField (IsSpecialContext context) context necessity nullability blueprint
