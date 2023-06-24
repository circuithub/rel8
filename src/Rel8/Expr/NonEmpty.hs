module Rel8.Expr.NonEmpty (
  head1Expr,
  index1Expr,
  last1Expr,
) where

-- base
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (fromPrimExpr, toPrimExpr)
import Rel8.Schema.Null (Nullify)


head1Expr :: Expr (NonEmpty a) -> Expr a
head1Expr array = fromPrimExpr $ toPrimExpr $ index1Expr array index
  where
    index = fromPrimExpr $ Opaleye.FunExpr "array_lower" [toPrimExpr array, one]
      where
        one = Opaleye.ConstExpr (Opaleye.IntegerLit 1)


index1Expr :: Expr (NonEmpty a) -> Expr Int64 -> Expr (Nullify a)
index1Expr array index =
  fromPrimExpr (Opaleye.ArrayIndex (toPrimExpr array) (toPrimExpr index))


last1Expr :: Expr (NonEmpty a) -> Expr a
last1Expr array = fromPrimExpr $ toPrimExpr $ index1Expr array index
  where
    index = fromPrimExpr $ Opaleye.FunExpr "array_upper" [toPrimExpr array, one]
      where
        one = Opaleye.ConstExpr (Opaleye.IntegerLit 1)
