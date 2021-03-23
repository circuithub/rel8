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
  , unsafeFromPrimExpr, unsafeToPrimExpr, unsafeMapPrimExpr
  , unsafeZipPrimExprsWith, unsafeTraversePrimExpr
  , sfromPrimExpr, stoPrimExpr
  , fromPrimExpr, toPrimExpr, mapPrimExpr, zipPrimExprsWith
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
import Rel8.Kind.Bool ( SBool( SFalse, STrue ), KnownBool, boolSing, IsList )
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
  Expr . Opaleye.CastExpr typeName . unsafeToPrimExpr


unsafeLiteral :: (DBType db, Nullabilizes db a) => String -> Expr a
unsafeLiteral = castExpr . Expr . Opaleye.ConstExpr . Opaleye.OtherLit


unsafeFromPrimExpr :: Opaleye.PrimExpr -> Expr a
unsafeFromPrimExpr = Expr


unsafeToPrimExpr :: Expr a -> Opaleye.PrimExpr
unsafeToPrimExpr (Expr a) = a


unsafeMapPrimExpr :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr) -> Expr a -> Expr b
unsafeMapPrimExpr f = unsafeFromPrimExpr . f . unsafeToPrimExpr


unsafeZipPrimExprsWith :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b -> Expr c
unsafeZipPrimExprsWith f a b =
  unsafeFromPrimExpr (f (unsafeToPrimExpr a) (unsafeToPrimExpr b))


unsafeTraversePrimExpr :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr) -> Expr a -> f (Expr b)
unsafeTraversePrimExpr f = fmap unsafeFromPrimExpr . f . unsafeToPrimExpr


fromPrimExpr ::
  ( KnownBool (IsList _a), Nullabilizes _a a
  )
  => Opaleye.PrimExpr -> Expr a
fromPrimExpr = sfromPrimExpr nullabilization boolSing


toPrimExpr ::
  ( KnownBool (IsList _a), Nullabilizes _a a
  )
  => Expr a -> Opaleye.PrimExpr
toPrimExpr = stoPrimExpr nullabilization boolSing


sfromPrimExpr :: ()
  => Nullability _a a -> SBool (IsList _a) -> Opaleye.PrimExpr -> Expr a
sfromPrimExpr _ = \case
  SFalse -> unsafeFromPrimExpr
  STrue -> \a -> Expr $
    Opaleye.CaseExpr
      [ (Opaleye.UnExpr Opaleye.OpIsNull a, Opaleye.ConstExpr Opaleye.NullLit)
      ]
      (Opaleye.UnExpr (Opaleye.UnOpOther "ROW") a)


stoPrimExpr :: ()
  => Nullability _a a -> SBool (IsList _a) -> Expr a -> Opaleye.PrimExpr
stoPrimExpr _ = \case
  SFalse -> unsafeToPrimExpr
  STrue -> \(Expr a) ->
    Opaleye.CaseExpr
      [ (Opaleye.UnExpr Opaleye.OpIsNull a, Opaleye.ConstExpr Opaleye.NullLit)
      ]
      -- Requires at least Postgres 13
      (Opaleye.CompositeExpr a "f1")


mapPrimExpr ::
  ( KnownBool (IsList _a), Nullabilizes _a a
  , KnownBool (IsList _b), Nullabilizes _b b
  )
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b
mapPrimExpr f = fromPrimExpr . f . toPrimExpr


zipPrimExprsWith ::
  ( KnownBool (IsList _a), Nullabilizes _a a
  , KnownBool (IsList _b), Nullabilizes _b b
  , KnownBool (IsList _c), Nullabilizes _c c
  )
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b -> Expr c
zipPrimExprsWith f a b = fromPrimExpr (f (toPrimExpr a) (toPrimExpr b))


toColumn :: Opaleye.PrimExpr -> Opaleye.Column b
toColumn = Opaleye.Column


fromColumn :: Opaleye.Column b -> Opaleye.PrimExpr
fromColumn (Opaleye.Column a) = a
