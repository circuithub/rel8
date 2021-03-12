{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB, unDB )
  , Insert( RequiredInsert, OptionalInsert )
  , Name( Name )
  , Labels( Labels )
  , Result( NonNullableResult, NullableResult )
  , IsSpecialContext
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.List.NonEmpty ( NonEmpty )
import Data.String ( IsString )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate )
import Rel8.Kind.Necessity
  ( Necessity( Optional, Required )
  , SNecessity( SOptional, SRequired )
  , KnownNecessity, necessitySing
  )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability, nullabilitySing
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )
import Rel8.Schema.Structure ( Structure )
import Rel8.Type.Monoid ( DBMonoid )
import Rel8.Type.Semigroup ( DBSemigroup )


type Aggregation :: Context
data Aggregation spec where
  Aggregation :: Aggregate nullability a -> Aggregation ('Spec necessity nullability a)


type DB :: Context
data DB spec where
  DB :: { unDB :: Expr nullability a } -> DB ('Spec necessity nullability a)


instance (spec ~ 'Spec necessity nullability a, DBSemigroup a) =>
  Semigroup (DB spec)
 where
  DB a <> DB b = DB (a <> b)


instance (spec ~ 'Spec necessity nullability a, DBMonoid a) =>
  Monoid (DB spec)
 where
  mempty = DB mempty


type Insert :: Context
data Insert spec where
  RequiredInsert :: ()
    => Expr nullability a -> Insert ('Spec 'Required nullability a)
  OptionalInsert :: ()
    => Maybe (Expr nullability a) -> Insert ('Spec 'Optional nullability a)


instance (spec ~ 'Spec necessity nullability a, DBSemigroup a) =>
  Semigroup (Insert spec)
 where
  RequiredInsert a <> RequiredInsert b = RequiredInsert (a <> b)
  OptionalInsert ma <> OptionalInsert mb = OptionalInsert (liftA2 (<>) ma mb)


instance
  ( spec ~ 'Spec necessity nullability a
  , KnownNecessity necessity
  , DBMonoid a
  ) => Monoid (Insert spec)
 where
  mempty = case necessitySing @necessity of
    SRequired -> RequiredInsert mempty
    SOptional -> OptionalInsert (Just mempty)


type Name :: Context
newtype Name spec = Name String
  deriving newtype (IsString, Monoid, Semigroup)


type Labels :: Context
newtype Labels spec = Labels (NonEmpty String)
  deriving newtype (Semigroup)


type Result :: Context
data Result spec where
  NonNullableResult :: a -> Result ('Spec necessity 'NonNullable a)
  NullableResult :: Maybe a -> Result ('Spec necessity 'Nullable a)


instance (spec ~ 'Spec necessity nullability a, Semigroup a) =>
  Semigroup (Result spec)
 where
  NonNullableResult a <> NonNullableResult b = NonNullableResult (a <> b)
  NullableResult ma <> NullableResult mb = NullableResult (liftA2 (<>) ma mb)


instance
  ( spec ~ 'Spec necessity nullability a
  , KnownNullability nullability
  , Monoid a
  ) => Monoid (Result spec)
 where
  mempty = case nullabilitySing @nullability of
    SNonNullable -> NonNullableResult mempty
    SNullable -> NullableResult (Just mempty)


type IsSpecialContext :: Context -> Bool
type family IsSpecialContext context where
  IsSpecialContext Aggregation = 'True
  IsSpecialContext DB = 'True
  IsSpecialContext Insert = 'True
  IsSpecialContext Result = 'True
  IsSpecialContext Structure = 'True
  IsSpecialContext _ = 'False
