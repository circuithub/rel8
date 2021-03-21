{-# language DataKinds #-}
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
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Blueprint ( Blueprint, ToDBType, ToType )
import Rel8.Kind.Labels ( Labels )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Kind.Nullability ( Nullability ( Nullable, NonNullable ) )
import Rel8.Schema.Context
  ( Aggregation, DB, Insertion, Result
  , IsSpecialContext
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )
import Rel8.Schema.Structure ( Structure, Shape( Column ), Shape1 )


type IField :: Bool -> Context -> Labels -> Necessity -> Nullability -> Blueprint -> Type
type family IField isSpecialContext labels context necessity nullability blueprint where
  IField 'False context     labels  necessity  nullability  blueprint = context ('Spec labels necessity nullability blueprint)
  IField 'True  Result      _labels _necessity 'NonNullable blueprint = ToType blueprint
  IField 'True  Result      _labels _necessity 'Nullable    blueprint = Maybe (ToType blueprint)
  IField 'True  DB          _labels _necessity nullability  blueprint = Expr nullability (ToDBType blueprint)
  IField 'True  Insertion   _labels 'Required  nullability  blueprint = Expr nullability (ToDBType blueprint)
  IField 'True  Insertion   _labels 'Optional  nullability  blueprint = Maybe (Expr nullability (ToDBType blueprint))
  IField 'True  Aggregation _labels _necessity nullability  blueprint = Aggregate (Expr nullability (ToDBType blueprint))
  IField 'True  Structure   labels necessity  nullability  blueprint = Shape1 'Column ('Spec labels necessity nullability blueprint)


type Field :: Context -> Labels -> Necessity -> Nullability -> Blueprint -> Type
type Field context labels necessity nullability blueprint =
  IField (IsSpecialContext context) context labels necessity nullability blueprint
