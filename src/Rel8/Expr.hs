{-# language DataKinds #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Expr
  ( Expr(..)
  , Column( ExprColumn, fromExprColumn )
  , binaryOperator
  , fromPrimExpr
  , column
  , traversePrimExpr
  , unsafeCastExpr
  , unsafeCoerceExpr
  , liftOpNull
  ) where

-- base
import Data.Kind ( Type )
import Data.String ( IsString( fromString ) )
import Prelude
  ( Applicative
  , Fractional( fromRational, (/) )
  , Maybe
  , Num( (+), (*), (-), signum, abs, fromInteger, negate )
  , String
  , ($)
  , (.)
  , fmap
  )

-- rel8
import qualified Opaleye ( PGInt8 )
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Context ( Context( Column ), Meta( Meta ) )
import Rel8.Expr.Opaleye ( columnToExpr, exprToColumn, litExpr )
import Rel8.Function ( function )
import Rel8.Info ( HasInfo )


-- | Typed SQL expressions
type Expr :: Type -> Type


type role Expr representational


newtype Expr a = Expr { toPrimExpr :: Opaleye.PrimExpr }


-- | It is assumed that any Haskell types that have a 'Num' instance also have
-- the corresponding operations in the database. Hence, Num a => Num (Expr a).
-- *However*, if this is not the case, you should `newtype` the Haskell type
-- and avoid providing a 'Num' instance, or you may write be able to write
-- ill-typed queries!
instance (HasInfo a, Num a) => Num (Expr a) where
  a + b = columnToExpr (Opaleye.binOp (Opaleye.:+) (exprToColumn a) (exprToColumn b))
  a - b = columnToExpr (Opaleye.binOp (Opaleye.:-) (exprToColumn a) (exprToColumn b))
  a * b = columnToExpr (Opaleye.binOp (Opaleye.:*) (exprToColumn a) (exprToColumn b))
  abs = function "abs"
  signum = columnToExpr @Opaleye.PGInt8 . signum . exprToColumn
  fromInteger = litExpr . fromInteger
  negate = columnToExpr @Opaleye.PGInt8 . negate . exprToColumn


instance (HasInfo a, Fractional a) => Fractional (Expr a) where
  a / b = columnToExpr (Opaleye.binOp (Opaleye.:/) (exprToColumn a) (exprToColumn b))
  fromRational = litExpr . fromRational


instance (IsString a, HasInfo a) => IsString (Expr a) where
  fromString = litExpr . fromString


-- | Cast an @Expr@ from one type to another.
unsafeCastExpr :: String -> Expr a -> Expr b
unsafeCastExpr t (Expr x) = Expr $ Opaleye.CastExpr t x


-- | Unsafely treat an 'Expr' that returns @a@s as returning @b@s.
unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr x) = Expr x


-- | Construct an expression by applying an infix binary operator to two
-- operands.
binaryOperator :: String -> Expr a -> Expr b -> Expr c
binaryOperator op (Expr a) (Expr b) = Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b


-- | Lift a binary operation on non-@null@ expressions to an equivalent binary
-- operator on possibly @null@ expressions.
-- 
-- Similar to @mapNull@, it is assumed that this binary operator will return
-- @null@ if either of its operands are @null@.
-- 
-- >>> select c $ pure $ liftOpNull (&&.) (lit (Just True)) (lit (Just False))
-- [Just False]
-- 
-- >>> select c $ pure $ liftOpNull (&&.) nullExpr (lit (Just False))
-- [Nothing]
-- 
-- This function can be thought of like 'liftA2'.
liftOpNull :: (Expr a -> Expr b -> Expr c) -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
liftOpNull f a b = unsafeCoerceExpr (f (unsafeCoerceExpr a) (unsafeCoerceExpr b))


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


column :: String -> Expr a
column columnName =
  Expr ( Opaleye.BaseTableAttrExpr columnName )


traversePrimExpr
  :: Applicative f
  => ( Opaleye.PrimExpr -> f Opaleye.PrimExpr ) -> Expr a -> f ( Expr a )
traversePrimExpr f =
  fmap fromPrimExpr . f . toPrimExpr


instance Context Expr where
  data Column Expr :: Meta -> Type where
    ExprColumn :: { fromExprColumn :: Expr a } -> Column Expr ('Meta defaulting a)
