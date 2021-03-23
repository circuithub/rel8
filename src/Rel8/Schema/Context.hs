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
  ( Insertion( RequiredInsert, OptionalInsert )
  , Name
  , Col( Name )
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
import Rel8.Schema.Spec ( Context, Spec( Spec ), Interpretation( Col ) )
import Rel8.Schema.Structure ( Structure )
import Rel8.Type.Monoid ( DBMonoid )
import Rel8.Type.Semigroup ( DBSemigroup )
import Data.Kind
import Data.Functor.Identity (Identity)


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


type Name :: Type -> Type
data Name a


instance Interpretation Name where
  data Col Name :: Spec -> Type where
    Name :: String -> Col Name spec


type IsSpecialContext :: (Type -> Type) -> Bool
type family IsSpecialContext context where
  IsSpecialContext Aggregate = 'True
  IsSpecialContext Expr = 'True
  -- IsSpecialContext Insert = 'True
  IsSpecialContext Identity = 'True
  -- IsSpecialContext Structure = 'True
  IsSpecialContext _ = 'False
