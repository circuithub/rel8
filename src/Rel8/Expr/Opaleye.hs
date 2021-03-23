{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Opaleye
  ( castExpr, unsafeCastExpr
  , scastExpr, sunsafeCastExpr
  , unsafeLiteral
  , fromPrimExpr, toPrimExpr, mapPrimExpr, zipPrimExprsWith, traversePrimExpr
  , toColumn, fromColumn
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Schema.Nullability ( Nullability, Nullabilizes, nullabilization )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation(..) )


castExpr :: (DBType db, Nullabilizes db a) => Expr a -> Expr a
castExpr = scastExpr nullabilization typeInformation


unsafeCastExpr :: (DBType db, Nullabilizes db b) => Expr a -> Expr b
unsafeCastExpr = sunsafeCastExpr nullabilization typeInformation


scastExpr :: Nullability db a -> TypeInformation db -> Expr a -> Expr a
scastExpr = sunsafeCastExpr


sunsafeCastExpr :: ()
  => Nullability db b -> TypeInformation db -> Expr a -> Expr b
sunsafeCastExpr _ TypeInformation {typeName} =
  fromPrimExpr . Opaleye.CastExpr typeName . toPrimExpr


unsafeLiteral :: (DBType db, Nullabilizes db a) => String -> Expr a
unsafeLiteral = castExpr . Expr . Opaleye.ConstExpr . Opaleye.OtherLit


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


toPrimExpr :: Expr a -> Opaleye.PrimExpr
toPrimExpr (Expr a) = a


mapPrimExpr :: (Opaleye.PrimExpr -> Opaleye.PrimExpr) -> Expr a -> Expr b
mapPrimExpr f = fromPrimExpr . f . toPrimExpr


zipPrimExprsWith :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b -> Expr c
zipPrimExprsWith f a b = fromPrimExpr (f (toPrimExpr a) (toPrimExpr b))


traversePrimExpr :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr) -> Expr a -> f (Expr b)
traversePrimExpr f = fmap fromPrimExpr . f . toPrimExpr


toColumn :: Opaleye.PrimExpr -> Opaleye.Column b
toColumn = Opaleye.Column


fromColumn :: Opaleye.Column b -> Opaleye.PrimExpr
fromColumn (Opaleye.Column a) = a
