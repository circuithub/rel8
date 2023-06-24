module Rel8.Expr.List (
  headExpr,
  indexExpr,
  lastExpr,
) where

-- base
import Data.Int (Int64)
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (fromPrimExpr, toPrimExpr)
import Rel8.Schema.Null (Nullify)


headExpr :: Expr [a] -> Expr (Nullify a)
headExpr array = indexExpr array index
  where
    index = fromPrimExpr $ Opaleye.FunExpr "array_lower" [toPrimExpr array, one]
      where
        one = Opaleye.ConstExpr (Opaleye.IntegerLit 1)


indexExpr :: Expr [a] -> Expr Int64 -> Expr (Nullify a)
indexExpr array index =
  fromPrimExpr (Opaleye.ArrayIndex (toPrimExpr array) (toPrimExpr index))


lastExpr :: Expr [a] -> Expr (Nullify a)
lastExpr array = indexExpr array index
  where
    index = fromPrimExpr $ Opaleye.FunExpr "array_upper" [toPrimExpr array, one]
      where
        one = Opaleye.ConstExpr (Opaleye.IntegerLit 1)
