{-# language NamedFieldPuns #-}

module Rel8.Expr.Opaleye
  ( castExpr, unsafeCastExpr
  , scastExpr, sunsafeCastExpr
  , unsafeLiteral
  , litPrimExpr
  , fromPrimExpr
  , toPrimExpr
  , mapPrimExpr
  , traversePrimExpr
  , zipPrimExprsWith
  , exprToColumn
  , columnToExpr
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Type ( DBType, TypeInformation(..), typeInformation )


castExpr :: DBType a => Expr nullability a -> Expr nullability a
castExpr = scastExpr typeInformation


unsafeCastExpr :: DBType b => Expr nullability a -> Expr nullability b
unsafeCastExpr = sunsafeCastExpr typeInformation


scastExpr :: ()
  => TypeInformation a -> Expr nullability a -> Expr nullability a
scastExpr = sunsafeCastExpr


sunsafeCastExpr :: ()
  => TypeInformation b -> Expr nullability a -> Expr nullability b
sunsafeCastExpr TypeInformation {typeName} =
  Expr . Opaleye.CastExpr typeName . toPrimExpr


unsafeLiteral :: DBType a => String -> Expr nullability a
unsafeLiteral = castExpr . Expr . Opaleye.ConstExpr . Opaleye.OtherLit


litPrimExpr :: DBType a => a -> Expr nullability a
litPrimExpr = slitPrimExpr typeInformation


slitPrimExpr :: TypeInformation a -> a -> Expr nullability a
slitPrimExpr info@TypeInformation {encode} = scastExpr info . Expr . encode


fromPrimExpr :: Opaleye.PrimExpr -> Expr nullability a
fromPrimExpr = Expr


toPrimExpr :: Expr nullability a -> Opaleye.PrimExpr
toPrimExpr (Expr a) = a


mapPrimExpr :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr nullability1 a -> Expr nullability2 b
mapPrimExpr f (Expr a) = Expr (f a)


traversePrimExpr :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr)
  -> Expr nullability1 a -> f (Expr nullability2 b)
traversePrimExpr f (Expr a) = Expr <$> f a


zipPrimExprsWith :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr nullability1 a -> Expr nullability2 b -> Expr nullability3 c
zipPrimExprsWith f (Expr a) (Expr b) = Expr (f a b)


exprToColumn :: Expr nullability a -> Opaleye.Column b
exprToColumn (Expr a) = Opaleye.Column a


columnToExpr :: Opaleye.Column b -> Expr nullability a
columnToExpr (Opaleye.Column a) = Expr a
