{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Expr.Opaleye
  ( toPrimExpr
  , fromPrimExpr
  , binExpr
  , exprToColumn
  , columnToExpr
  , zipPrimExprsWith
  , mapPrimExpr
  , unsafeLiteral
  , litExprWith
  , litExpr
  , traversePrimExpr
  , unsafeCastExpr
  , binaryOperator
  , column
  , unsafeCoerceExpr
  , recast
  , unsafeNullExpr
  ) where

-- rel8
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DatabaseType ( DatabaseType( DatabaseType, encode, typeName ) )
import Rel8.Expr ( Expr( Expr, toPrimExpr ) )
import Rel8.Info ( HasInfo( info ), Info( NotNull, Null ) )


binExpr :: Opaleye.BinOp -> Expr a -> Expr a -> Expr b
binExpr op (Expr a) (Expr b) = Expr $ Opaleye.BinExpr op a b 


exprToColumn :: Expr a -> Opaleye.Column b
exprToColumn (Expr a) = Opaleye.Column a


columnToExpr :: Opaleye.Column b -> Expr a
columnToExpr (Opaleye.Column a) = Expr a


mapPrimExpr :: (Opaleye.PrimExpr -> Opaleye.PrimExpr) -> Expr x -> Expr y
mapPrimExpr f (Expr x) = Expr $ f x


zipPrimExprsWith
  :: (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b -> Expr c
zipPrimExprsWith f (Expr x) (Expr y) = Expr $ f x y


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
  NotNull DatabaseType{ encode, typeName } -> unsafeCastExpr typeName . Expr . encode
  Null DatabaseType{ encode, typeName }    -> unsafeCastExpr typeName . maybe unsafeNullExpr (Expr . encode)


-- | Cast an @Expr@ from one type to another.
unsafeCastExpr :: String -> Expr a -> Expr b
unsafeCastExpr t (Expr x) = Expr $ Opaleye.CastExpr t x


-- | Add an explicit cast to an Expr.
recast :: forall a. HasInfo a => Expr a -> Expr a
recast = unsafeCastExpr case info @a of
  NotNull DatabaseType{ typeName } -> typeName
  Null DatabaseType{ typeName } -> typeName


-- | Unsafely treat an 'Expr' that returns @a@s as returning @b@s.
unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr x) = Expr x


-- | Construct an expression by applying an infix binary operator to two
-- operands.
binaryOperator :: String -> Expr a -> Expr b -> Expr c
binaryOperator op (Expr a) (Expr b) = Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b


column :: String -> Expr a
column columnName = Expr $ Opaleye.BaseTableAttrExpr columnName 


traversePrimExpr :: Applicative f => (Opaleye.PrimExpr -> f Opaleye.PrimExpr) -> Expr a -> f (Expr a)
traversePrimExpr f = fmap fromPrimExpr . f . toPrimExpr


unsafeNullExpr :: Expr a
unsafeNullExpr = Expr $ Opaleye.ConstExpr Opaleye.NullLit
