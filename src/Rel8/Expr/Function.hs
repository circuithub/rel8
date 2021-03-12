{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Expr.Function
  ( Function, function
  , nullaryFunction
  , binaryOperator
  , binaryOperatorOpaleye
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ), castExpr, toPrimExpr )
import Rel8.Type ( DBType )


type Function :: Type -> Type -> Constraint
class Function arg res where
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res


instance (arg ~ Expr nullability a, DBType a) => Function arg (Expr nullability a) where
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
binaryOperator = binaryOperatorOpaleye . Opaleye.OpOther


binaryOperatorOpaleye :: DBType c
  => Opaleye.BinOp
  -> Expr nullabilityA a
  -> Expr nullabilityB b
  -> Expr nullabilityC c
binaryOperatorOpaleye op (Expr a) (Expr b) =
  castExpr $ Expr $ Opaleye.BinExpr op a b
