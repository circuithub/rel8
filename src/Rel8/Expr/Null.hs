{-# options_ghc -Wno-orphans #-}

module Rel8.Expr.Null
  ( null
  , isNull
  , nullExpr
  , liftNull
  , mapNull
  , fromNull
  , liftOpNull
  ) where

-- base
import Prelude ( Bool( False ), Maybe( Nothing ), (.), id )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DBType ( DBType )
import Rel8.DBType.DBEq ( DBEq( (==.) ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (||.), ifThenElse_ )
import Rel8.Expr.Opaleye ( litExpr, mapPrimExpr, unsafeCoerceExpr )


-- | Like 'maybe', but to eliminate @null@.
--
-- >>> select c $ pure $ null 0 id (nullExpr :: Expr (Maybe Int32))
-- [0]
--
-- >>> select c $ pure $ null 0 id (lit (Just 42) :: Expr (Maybe Int32))
-- [42]
null :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
null whenNull f a = ifThenElse_ (isNull a) whenNull (f (unsafeCoerceExpr a))


-- | Like 'isNothing', but for @null@.
--
-- >>> select c $ pure $ isNull (nullExpr :: Expr (Maybe Int32))
-- [True]
--
-- >>> select c $ pure $ isNull (lit (Just 42) :: Expr (Maybe Int32))
-- [False]
isNull :: Expr (Maybe a) -> Expr Bool
isNull = mapPrimExpr ( Opaleye.UnExpr Opaleye.OpIsNull )


-- | Corresponds to SQL @null@.
nullExpr :: DBType a => Expr (Maybe a)
nullExpr = litExpr Nothing


-- | Lift an expression that's not null to a type that might be @null@. This is
-- an identity operation in terms of any generated query, and just modifies the
-- query's type.
liftNull :: Expr a -> Expr (Maybe a)
liftNull = unsafeCoerceExpr


-- | Lift an operation on non-@null@ values to an operation on possibly @null@
-- values.
-- 
-- @mapNull@ requires that the supplied function "preserves nulls", as no
-- actual case analysis is done (instead the @Expr (Maybe a)@ is simply unsafeCoerceExprd
-- and assumed to not be @null@). In most cases, this is true, but this
-- contract can be violated with custom functions.
mapNull :: (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
mapNull f = unsafeCoerceExpr . f . unsafeCoerceExpr


fromNull :: Expr a -> Expr (Maybe a) -> Expr a
fromNull x = null x id


instance (DBType a, DBEq a) => DBEq (Maybe a) where
  a ==. b =
    null ( isNull b ) ( \a' -> null ( litExpr False ) ( a' ==. ) b ) a


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
liftOpNull
  :: DBType c
  => (Expr a -> Expr b -> Expr c)
  -> Expr (Maybe a)
  -> Expr (Maybe b)
  -> Expr (Maybe c)
liftOpNull f a b =
  ifThenElse_
    (isNull a ||. isNull b)
    nullExpr
    (unsafeCoerceExpr (f (unsafeCoerceExpr a) (unsafeCoerceExpr b)))
