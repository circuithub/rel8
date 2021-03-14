{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language RoleAnnotations #-}
{-# language StandaloneKindSignatures #-}

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
import Rel8.Expr.Null ( seminullify, liftOpNullable )
import Rel8.Expr.Opaleye
  ( castExpr
  , litPrimExpr
  , mapPrimExpr
  , zipPrimExprsWith
  )
import Rel8.Kind.Nullability ( Nullability )
import Rel8.Type ( DBType )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Num ( DBFractional, DBNum )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )


type role Expr representational representational
type Expr :: Nullability -> Type -> Type
newtype Expr nullability a = Expr Opaleye.PrimExpr
  deriving stock Show


instance DBSemigroup a => Semigroup (Expr nullability a) where
  (<>) = liftOpNullable (<>.)


instance DBMonoid a => Monoid (Expr nullability a) where
  mempty = seminullify memptyExpr


instance (IsString a, DBType a) => IsString (Expr nullability a) where
  fromString = litPrimExpr . fromString


instance DBNum a => Num (Expr nullability a) where
  (+) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:+))
  (*) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:*))
  (-) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:-))

  abs = mapPrimExpr (Opaleye.UnExpr Opaleye.OpAbs)
  negate = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNegate)

  signum = castExpr . mapPrimExpr (Opaleye.UnExpr (Opaleye.UnOpOther "SIGN"))

  fromInteger = castExpr . Expr . Opaleye.ConstExpr . Opaleye.IntegerLit


instance DBFractional a => Fractional (Expr nullability a) where
  (/) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:/))

  fromRational =
    castExpr . Expr . Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
