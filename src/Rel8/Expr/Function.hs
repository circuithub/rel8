{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}

module Rel8.Expr.Function ( Function, function, nullaryFunction ) where

import Rel8.Expr ( Expr( Expr ) )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


-- | The @Function@ type class is an implementation detail that allows
-- @function@ to be polymorphic in the number of arguments it consumes.
class Function arg res where
  -- | Build a function of multiple arguments.
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res
  applyArgument =
    -- We do need 'applyArgument', but if we don't specify this and let GHC
    -- infer the minimal contract, it leaks out into documentation. This is a
    -- private class, so we don't want to document anything more than its
    -- existance.
    undefined


instance arg ~ Expr a => Function arg (Expr res) where
  applyArgument mkExpr (Expr a) = Expr $ mkExpr [a]


instance (arg ~ Expr a, Function args res) => Function arg (args -> res) where
  applyArgument f (Expr a) = applyArgument (f . (a :))


-- | Construct an n-ary function that produces an 'Expr' that when called runs
-- a SQL function.
-- 
-- For example, here's how we can wrap PostgreSQL's @factorial@ function:
-- 
-- >>> :{
-- factorial :: Expr Int64 -> Expr Data.Scientific.Scientific
-- factorial = function "factorial"
-- :}
-- 
-- >>> select c $ pure $ factorial 5
-- [120.0]
-- 
-- The same approach works for any number of arguments:
-- 
-- >>> :{
-- power :: Expr Float -> Expr Float -> Expr Double
-- power = function "power"
-- :}
-- 
-- >>> select c $ pure $ power 9 3
-- [729.0]
function :: Function args result => String -> args -> result
function = applyArgument . Opaleye.FunExpr


-- | Construct a function call for functions with no arguments.
-- 
-- For example, we can call the database function @pi()@ by using
-- @nullaryFunction@:
-- 
-- >>> :{
-- sqlPi :: Expr Double
-- sqlPi = nullaryFunction "pi"
-- :}
-- 
-- >>> select c $ pure $ sqlPi
-- [3.141592653589793]
nullaryFunction :: String -> Expr a
nullaryFunction name = Expr (Opaleye.FunExpr name [])
