{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB, unDB )
  , Insertion( RequiredInsert, OptionalInsert )
  , Name( Name )
  , Result( Result )
  , IsSpecialContext
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.String ( IsString )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Necessity
  ( Necessity( Optional, Required )
  , SNecessity( SOptional, SRequired )
  , KnownNecessity, necessitySing
  )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )
import Rel8.Schema.Structure ( Structure )
import Rel8.Type.Monoid ( DBMonoid )
import Rel8.Type.Semigroup ( DBSemigroup )


type Aggregation :: Context
data Aggregation spec where
  Aggregation :: ()
    => Aggregate (Expr a)
    -> Aggregation ('Spec labels necessity dbType a)


type DB :: Context
data DB spec where
  DB :: ()
    => { unDB :: Expr a }
    -> DB ('Spec labels necessity dbType a)
deriving stock instance Show (DB spec)


instance (spec ~ 'Spec labels necessity dbType a, Sql DBSemigroup a) =>
  Semigroup (DB spec)
 where
  DB a <> DB b = DB (a <> b)


instance (spec ~ 'Spec labels necessity dbType a, Sql DBMonoid a) =>
  Monoid (DB spec)
 where
  mempty = DB mempty


type Insertion :: Context
data Insertion spec where
  RequiredInsert :: ()
    => Expr a
    -> Insertion ('Spec labels 'Required dbType a)
  OptionalInsert :: ()
    => Maybe (Expr a)
    -> Insertion ('Spec labels 'Optional dbType a)
deriving stock instance Show (Insertion spec)


instance (spec ~ 'Spec labels necessity dbType a, Sql DBSemigroup a) =>
  Semigroup (Insertion spec)
 where
  RequiredInsert a <> RequiredInsert b = RequiredInsert (a <> b)
  OptionalInsert ma <> OptionalInsert mb = OptionalInsert (liftA2 (<>) ma mb)


instance
  ( spec ~ 'Spec labels necessity dbType a
  , KnownNecessity necessity
  , Sql DBMonoid a
  ) => Monoid (Insertion spec)
 where
  mempty = case necessitySing @necessity of
    SRequired -> RequiredInsert mempty
    SOptional -> OptionalInsert (Just mempty)


type Name :: Context
newtype Name spec = Name String
  deriving stock Show
  deriving newtype (IsString, Monoid, Semigroup)


type Result :: Context
data Result spec where
  Result :: a -> Result ('Spec labels necessity dbType a)
deriving stock instance Show a =>
  Show (Result ('Spec labels necessity dbType a))


instance (spec ~ 'Spec labels necessity dbType a, Semigroup a) =>
  Semigroup (Result spec)
 where
  Result a <> Result b = Result (a <> b)


instance (spec ~ 'Spec labels necessity dbType a, Monoid a) =>
  Monoid (Result spec)
 where
  mempty = Result mempty


type IsSpecialContext :: Context -> Bool
type family IsSpecialContext context where
  IsSpecialContext Aggregation = 'True
  IsSpecialContext DB = 'True
  IsSpecialContext Insertion = 'True
  IsSpecialContext Result = 'True
  IsSpecialContext Structure = 'True
  IsSpecialContext _ = 'False
