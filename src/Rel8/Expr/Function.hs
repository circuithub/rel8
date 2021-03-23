{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Expr.Function
  ( Function, function
  , nullaryFunction
  , binaryOperator
  , unsafeBinaryOperator
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr, toPrimExpr, zipPrimExprsWith
  , unsafeZipPrimExprsWith
  )
import Rel8.Kind.Bool ( KnownBool, IsList )
import Rel8.Schema.Nullability ( Nullabilizes )
import Rel8.Type ( DBType )


type Function :: Type -> Type -> Constraint
class Function arg res where
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res


instance
  ( arg ~ Expr a
  , Nullabilizes _a a, KnownBool (IsList _a)
  , DBType _b, Nullabilizes _b b, KnownBool (IsList _b)
  )
  => Function arg (Expr b)
 where
  applyArgument f a = castExpr $ fromPrimExpr $ f [toPrimExpr a]


instance
  ( arg ~ Expr a
  , Nullabilizes _a a, KnownBool (IsList _a)
  , Function args res
  ) =>
  Function arg (args -> res)
 where
  applyArgument f a = applyArgument (f . (toPrimExpr a :))


function :: Function args result => String -> args -> result
function = applyArgument . Opaleye.FunExpr


nullaryFunction :: (DBType db, Nullabilizes db a) => String -> Expr a
nullaryFunction name = castExpr $ Expr (Opaleye.FunExpr name [])


binaryOperator ::
  ( Nullabilizes _a a, KnownBool (IsList _a)
  , Nullabilizes _b b, KnownBool (IsList _b)
  , DBType _c, Nullabilizes _c c, KnownBool (IsList _c)
  )
  => String -> Expr a -> Expr b -> Expr c
binaryOperator operator a b =
  castExpr $ zipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther operator)) a b


unsafeBinaryOperator :: (DBType _c, Nullabilizes _c c)
  => String -> Expr a -> Expr b -> Expr c
unsafeBinaryOperator operator a b =
  castExpr $ unsafeZipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther operator)) a b
