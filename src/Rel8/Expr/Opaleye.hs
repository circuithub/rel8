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
import {-# source #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Type ( DBType, TypeInformation(..), typeInformation )


castExpr :: DBType a => Expr a -> Expr a
castExpr = scastExpr typeInformation


unsafeCastExpr :: DBType b => Expr a -> Expr b
unsafeCastExpr = sunsafeCastExpr typeInformation


scastExpr :: ()
  => TypeInformation a -> Expr a -> Expr a
scastExpr = sunsafeCastExpr


sunsafeCastExpr :: ()
  => TypeInformation b -> Expr a -> Expr b
sunsafeCastExpr TypeInformation {typeName} =
  Expr . Opaleye.CastExpr typeName . toPrimExpr


unsafeLiteral :: DBType a => String -> Expr a
unsafeLiteral = castExpr . Expr . Opaleye.ConstExpr . Opaleye.OtherLit


litPrimExpr :: DBType a => a -> Expr a
litPrimExpr = slitPrimExpr typeInformation


slitPrimExpr :: TypeInformation a -> a -> Expr a
slitPrimExpr info@TypeInformation {encode} = scastExpr info . Expr . encode


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


toPrimExpr :: Expr a -> Opaleye.PrimExpr
toPrimExpr (Expr a) = a


mapPrimExpr :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b
mapPrimExpr f (Expr a) = Expr (f a)


traversePrimExpr :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr)
  -> Expr a -> f (Expr b)
traversePrimExpr f (Expr a) = Expr <$> f a


zipPrimExprsWith :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b -> Expr c
zipPrimExprsWith f (Expr a) (Expr b) = Expr (f a b)


exprToColumn :: Expr a -> Opaleye.Column b
exprToColumn (Expr a) = Opaleye.Column a


columnToExpr :: Opaleye.Column b -> Expr a
columnToExpr (Opaleye.Column a) = Expr a
