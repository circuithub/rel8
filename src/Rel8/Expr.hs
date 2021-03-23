{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Rel8.Expr
  ( Expr(..)
  , Col( DB, unDB )
  )
where

-- base
import Data.Kind ( Type )
import Data.String ( IsString, fromString )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr.Null ( liftOpNullable, nullify )
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr
  , mapPrimExpr
  , zipPrimExprsWith
  )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Nullability
  ( Nullability( Nullable, NonNullable )
  , Sql, nullabilization
  )
import Rel8.Type ( DBType )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Num ( DBFractional, DBNum )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )
import Rel8.Schema.Spec


type role Expr representational
type Expr :: Type -> Type
newtype Expr a = Expr Opaleye.PrimExpr
  deriving stock Show


instance Sql DBSemigroup a => Semigroup (Expr a) where
  (<>) = case nullabilization @a of
    Nullable -> liftOpNullable (<>.)
    NonNullable -> (<>.)
  {-# INLINABLE (<>) #-}


instance Sql DBMonoid a => Monoid (Expr a) where
  mempty = case nullabilization @a of
    Nullable -> nullify memptyExpr
    NonNullable -> memptyExpr
  {-# INLINABLE mempty #-}


instance (Sql IsString a, Sql DBType a) => IsString (Expr a) where
  fromString = litExpr . case nullabilization @a of
    Nullable -> Just . fromString
    NonNullable -> fromString


instance Sql DBNum a => Num (Expr a) where
  (+) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:+))
  (*) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:*))
  (-) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:-))

  abs = mapPrimExpr (Opaleye.UnExpr Opaleye.OpAbs)
  negate = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNegate)

  signum = castExpr . mapPrimExpr (Opaleye.UnExpr (Opaleye.UnOpOther "SIGN"))

  fromInteger = castExpr . fromPrimExpr . Opaleye.ConstExpr . Opaleye.IntegerLit


instance Sql DBFractional a => Fractional (Expr a) where
  (/) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:/))

  fromRational =
    castExpr . Expr . Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac


instance Interpretation Expr where
  data Col Expr :: Spec -> Type where
    DB :: ()
      => { unDB :: Expr a }
      -> Col Expr ('Spec labels necessity dbType a)


instance (spec ~ 'Spec labels necessity dbType a, Sql DBSemigroup a) =>
  Semigroup (Col Expr spec)
 where
  DB a <> DB b = DB (a <> b)


instance (spec ~ 'Spec labels necessity dbType a, Sql DBMonoid a) =>
  Monoid (Col Expr spec)
 where
  mempty = DB mempty
