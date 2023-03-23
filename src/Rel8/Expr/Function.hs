{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Expr.Function
  ( Function, function
  , nullaryFunction
  , binaryOperator
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr, toPrimExpr, zipPrimExprsWith
  )
import Rel8.Schema.Null ( Sql )
import Rel8.Type ( DBType )


-- | This type class exists to allow 'function' to have arbitrary arity. It's
-- mostly an implementation detail, and typical uses of 'Function' shouldn't
-- need this to be specified.
type Function :: Type -> Type -> Constraint
class Function arg res where
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res


instance (arg ~ Expr a, Sql DBType b) => Function arg (Expr b) where
  applyArgument f a = castExpr $ fromPrimExpr $ f [toPrimExpr a]


instance (arg ~ Expr a, Function args res) => Function arg (args -> res) where
  applyArgument f a = applyArgument (f . (toPrimExpr a :))


-- | Construct an n-ary function that produces an 'Expr' that when called runs
-- a SQL function.
function :: Function args result => String -> args -> result
function = applyArgument . Opaleye.FunExpr


-- | Construct a function call for functions with no arguments.
nullaryFunction :: Sql DBType a => String -> Expr a
nullaryFunction name = castExpr $ Expr (Opaleye.FunExpr name [])


-- | Construct an expression by applying an infix binary operator to two
-- operands.
binaryOperator :: Sql DBType c => String -> Expr a -> Expr b -> Expr c
binaryOperator operator a b =
  castExpr $ zipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther operator)) a b
