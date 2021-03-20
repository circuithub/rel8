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
  , litPrimExpr
  , unsafeFromPrimExpr, unsafeToPrimExpr, unsafeMapPrimExpr
  , unsafeZipPrimExprsWith, unsafeTraversePrimExpr
  , sfromPrimExpr, stoPrimExpr
  , fromPrimExpr, toPrimExpr, mapPrimExpr, zipPrimExprsWith
  , exprToColumn
  , columnToExpr
  )
where

-- base
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Blueprint
  ( SBlueprint( SScalar, SVector )
  , ToDBType
  , blueprintRoundtripsViaDBType
  )
import Rel8.Type ( DBType, blueprintForDBType, typeInformation )
import Rel8.Type.Information ( TypeInformation(..) )


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
  Expr . Opaleye.CastExpr typeName . unsafeToPrimExpr


unsafeLiteral :: DBType a => String -> Expr nullability a
unsafeLiteral = castExpr . Expr . Opaleye.ConstExpr . Opaleye.OtherLit


litPrimExpr :: DBType a => a -> Expr nullability a
litPrimExpr = slitPrimExpr typeInformation


slitPrimExpr :: TypeInformation a -> a -> Expr nullability a
slitPrimExpr info@TypeInformation {encode} = scastExpr info . Expr . encode


unsafeFromPrimExpr :: Opaleye.PrimExpr -> Expr nullability a
unsafeFromPrimExpr = Expr


unsafeToPrimExpr :: Expr nullability a -> Opaleye.PrimExpr
unsafeToPrimExpr (Expr a) = a


unsafeMapPrimExpr :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr nullability1 a -> Expr nullability2 b
unsafeMapPrimExpr f = unsafeFromPrimExpr . f . unsafeToPrimExpr


unsafeZipPrimExprsWith :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr nullability1 a -> Expr nullability2 b -> Expr nullability3 c
unsafeZipPrimExprsWith f a b =
  unsafeFromPrimExpr (f (unsafeToPrimExpr a) (unsafeToPrimExpr b))


unsafeTraversePrimExpr :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr)
  -> Expr nullability1 a -> f (Expr nullability2 b)
unsafeTraversePrimExpr f = fmap unsafeFromPrimExpr . f . unsafeToPrimExpr


fromPrimExpr :: forall a nullability. DBType a
  => Opaleye.PrimExpr -> Expr nullability a
fromPrimExpr = case blueprintForDBType @a of
  blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
    Refl -> sfromPrimExpr blueprint


toPrimExpr :: forall a nullability. DBType a
  => Expr nullability a -> Opaleye.PrimExpr
toPrimExpr = case blueprintForDBType @a of
  blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
    Refl -> stoPrimExpr blueprint


sfromPrimExpr :: a ~ ToDBType blueprint
  => SBlueprint blueprint -> Opaleye.PrimExpr -> Expr nullability a
sfromPrimExpr = \case
  SScalar _ -> unsafeFromPrimExpr
  SVector {} -> \a -> Expr $
    Opaleye.CaseExpr
      [ (Opaleye.UnExpr Opaleye.OpIsNull a, Opaleye.ConstExpr Opaleye.NullLit)
      ]
      (Opaleye.UnExpr (Opaleye.UnOpOther "ROW") a)


stoPrimExpr :: a ~ ToDBType blueprint
  => SBlueprint blueprint -> Expr nullability a -> Opaleye.PrimExpr
stoPrimExpr = \case
  SScalar _ -> unsafeToPrimExpr
  SVector {} -> \(Expr a) ->
    Opaleye.CaseExpr
      [ (Opaleye.UnExpr Opaleye.OpIsNull a, Opaleye.ConstExpr Opaleye.NullLit)
      ]
      -- Requires at least Postgres 13
      (Opaleye.CompositeExpr a "f1")


mapPrimExpr :: (DBType a, DBType b)
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr nullability1 a -> Expr nullability2 b
mapPrimExpr f = fromPrimExpr . f . toPrimExpr


zipPrimExprsWith :: (DBType a, DBType b, DBType c)
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr nullability1 a -> Expr nullability2 b -> Expr nullability3 c
zipPrimExprsWith f a b = fromPrimExpr (f (toPrimExpr a) (toPrimExpr b))


exprToColumn :: Expr nullability a -> Opaleye.Column b
exprToColumn (Expr a) = Opaleye.Column a


columnToExpr :: Opaleye.Column b -> Expr nullability a
columnToExpr (Opaleye.Column a) = Expr a
