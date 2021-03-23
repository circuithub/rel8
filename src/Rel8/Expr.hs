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
  , unsafeFromPrimExpr
  , mapPrimExpr
  , zipPrimExprsWith
  )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Nullability
  ( Nullability( Nullable, NonNullable )
  , Nullabilizes, nullabilization
  )
import Rel8.Type ( DBType )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Num ( DBFractional, DBNum )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )


type role Expr representational
type Expr :: Type -> Type
newtype Expr a = Expr Opaleye.PrimExpr
  deriving stock Show


instance (DBSemigroup db, Nullabilizes db a) => Semigroup (Expr a) where
  (<>) = case nullabilization @a of
    Nullable -> liftOpNullable (<>.)
    NonNullable -> (<>.)


instance (DBMonoid db, Nullabilizes db a) => Monoid (Expr a) where
  mempty = case nullabilization @a of
    Nullable -> nullify memptyExpr
    NonNullable -> memptyExpr


instance (IsString db, DBType db, Nullabilizes db a) => IsString (Expr a) where
  fromString = litExpr . case nullabilization @a of
    Nullable -> Just . fromString
    NonNullable -> fromString


instance (DBNum db, Nullabilizes db a) => Num (Expr a) where
  (+) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:+))
  (*) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:*))
  (-) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:-))

  abs = mapPrimExpr (Opaleye.UnExpr Opaleye.OpAbs)
  negate = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNegate)

  signum = castExpr . mapPrimExpr (Opaleye.UnExpr (Opaleye.UnOpOther "SIGN"))

  fromInteger = castExpr . unsafeFromPrimExpr . Opaleye.ConstExpr . Opaleye.IntegerLit


instance (DBFractional db, Nullabilizes db a) => Fractional (Expr a) where
  (/) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:/))

  fromRational =
    castExpr . Expr . Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
