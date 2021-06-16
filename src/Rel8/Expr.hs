{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Expr
  ( Expr(..)
  , Col( E, unE )
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )
import Data.String ( IsString, fromString )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr.Function ( function, nullaryFunction )
import Rel8.Expr.Null ( liftOpNull, nullify )
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr
  , mapPrimExpr
  , zipPrimExprsWith
  )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Context ( Interpretation, Col )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ), HType )
import Rel8.Schema.Null ( Nullity( Null, NotNull ), Sql, nullable )
import Rel8.Schema.Reify ( notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns, reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Num ( DBFloating, DBFractional, DBNum )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )


-- | Typed SQL expressions.
type role Expr representational
type Expr :: k -> Type
data Expr a where
  Expr :: k ~ Type => !Opaleye.PrimExpr -> Expr (a :: k)


deriving stock instance Show (Expr a)


instance Sql DBSemigroup a => Semigroup (Expr a) where
  (<>) = case nullable @a of
    Null -> liftOpNull (<>.)
    NotNull -> (<>.)
  {-# INLINABLE (<>) #-}


instance Sql DBMonoid a => Monoid (Expr a) where
  mempty = case nullable @a of
    Null -> nullify memptyExpr
    NotNull -> memptyExpr
  {-# INLINABLE mempty #-}


instance (Sql IsString a, Sql DBType a) => IsString (Expr a) where
  fromString = litExpr . case nullable @a of
    Null -> Just . fromString
    NotNull -> fromString


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


instance Sql DBFloating a => Floating (Expr a) where
  pi = nullaryFunction "PI"
  exp = function "exp"
  log = function "ln"
  sqrt = function "sqrt"
  (**) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:^))
  logBase = function "log"
  sin = function "sin"
  cos = function "cos"
  tan = function "tan"
  asin = function "asin"
  acos = function "acos"
  atan = function "atan"
  sinh = function "sinh"
  cosh = function "cosh"
  tanh = function "tanh"
  asinh = function "asinh"
  acosh = function "acosh"
  atanh = function "atanh"


instance Interpretation Expr where
  data Col Expr _spec where
    E :: {unE :: !(Expr a)} -> Col Expr ('Spec labels defaulting a)


instance Sql DBType a => Table Expr (Expr a) where
  type Columns (Expr a) = HType a
  type Context (Expr a) = Expr

  toColumns a = HType (E a)
  fromColumns (HType (E a)) = a
  reify = notReify
  unreify = notReify


instance Sql DBType a => Recontextualize Expr Expr (Expr a) (Expr a)


instance Sql DBType a => Recontextualize Expr Result (Expr a) (Identity a)


instance Sql DBType a => Recontextualize Result Expr (Identity a) (Expr a)


instance Labelable Expr where
  labeler (E a) = E a
  unlabeler (E a) = E a
