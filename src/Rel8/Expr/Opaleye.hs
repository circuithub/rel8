{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Expr.Opaleye
  ( binExpr
  , exprToColumn
  , columnToExpr
  , zipPrimExprsWith
  , mapPrimExpr
  , unsafeLiteral
  , litExprWith
  , litExpr
  ) where

-- rel8
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DatabaseType ( DatabaseType( DatabaseType, encode, typeName ) )
import Rel8.Info ( HasInfo( info ), Info( NotNull, Null ) )
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


-- | Construct a SQL expression from some literal text. The provided literal
-- will be interpolated exactly as specified with no escaping.
unsafeLiteral :: forall a. String -> Expr a
unsafeLiteral = fromPrimExpr . Opaleye.ConstExpr . Opaleye.OtherLit


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


litExpr :: HasInfo a => a -> Expr a
litExpr = litExprWith info


litExprWith :: Info a -> a -> Expr a
litExprWith = \case
  NotNull DatabaseType{ encode, typeName } -> Expr . Opaleye.CastExpr typeName . encode
  Null DatabaseType{ encode, typeName }    -> Expr . Opaleye.CastExpr typeName . maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
