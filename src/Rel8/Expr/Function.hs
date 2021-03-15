{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

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
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye ( castExpr, toPrimExpr, zipPrimExprsWith )
import Rel8.Type ( DBType )


type Function :: Type -> Type -> Constraint
class Function arg res where
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res


instance (arg ~ Expr nullability a, DBType b) => Function arg (Expr nullability b) where
  applyArgument f a = castExpr $ Expr $ f [toPrimExpr a]


instance (arg ~ Expr nullability a, Function args res) => Function arg (args -> res) where
  applyArgument f a = applyArgument (f . (toPrimExpr a :))


function :: Function args result => String -> args -> result
function = applyArgument . Opaleye.FunExpr


nullaryFunction :: DBType a => String -> Expr nullability a
nullaryFunction name = castExpr $ Expr (Opaleye.FunExpr name [])


binaryOperator :: DBType c
  => String
  -> Expr nullabilityA a -> Expr nullabilityB b -> Expr nullabilityC c
binaryOperator operator a b =
  castExpr $ zipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther operator)) a b
