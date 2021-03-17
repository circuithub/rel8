module Rel8.Expr.Opaleye
  ( binExpr
  , exprToColumn
  , columnToExpr
  , zipPrimExprsWith
  , mapPrimExpr
  ) where

-- rel8
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import {-# source #-} Rel8.Expr ( Expr( Expr ) )


binExpr :: Opaleye.BinOp -> Expr a -> Expr a -> Expr b
binExpr op ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr op a b )


exprToColumn :: Expr a -> Opaleye.Column b
exprToColumn (Expr a) = Opaleye.Column a


columnToExpr :: Opaleye.Column b -> Expr a
columnToExpr (Opaleye.Column a) = Expr a


mapPrimExpr :: (Opaleye.PrimExpr -> Opaleye.PrimExpr) -> Expr x -> Expr y
mapPrimExpr f (Expr x) = Expr (f x)


zipPrimExprsWith
  :: (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b -> Expr c
zipPrimExprsWith f (Expr x) (Expr y) = Expr (f x y)
