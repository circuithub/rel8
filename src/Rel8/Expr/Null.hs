{-# language DataKinds #-}
{-# language ViewPatterns #-}

module Rel8.Expr.Null
  ( null, snull, nullable, nullableOf
  , isNull, isNonNull
  , nullify, seminullify
  , mapNullable, liftOpNullable
  , unsafeUnnullify, unsafeMapNullable, unsafeLiftOpNullable
  )
where

-- base
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Bool ( boolExpr, (||.) )
import Rel8.Expr.Opaleye ( scastExpr, mapPrimExpr )
import Rel8.Kind.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Type ( DBType, TypeInformation, typeInformation )


nullable :: ()
  => Expr nullability b
  -> (Expr 'NonNullable a -> Expr nullability b)
  -> Expr 'Nullable a
  -> Expr nullability b
nullable b f ma = boolExpr (f (unsafeUnnullify ma)) b (isNull ma)


nullableOf :: DBType a => Maybe (Expr 'NonNullable a) -> Expr 'Nullable a
nullableOf = maybe null nullify


isNull :: Expr 'Nullable a -> Expr 'NonNullable Bool
isNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNull)


isNonNull :: Expr 'Nullable a -> Expr 'NonNullable Bool
isNonNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNotNull)


mapNullable :: DBType b
  => (Expr nullability' a -> Expr nullability' b)
  -> Expr nullability a -> Expr nullability b
mapNullable f ma =
  boolExpr (unsafeMapNullable f ma) (unsafeUnnullify null)
    (isNull (nullify ma))


liftOpNullable :: DBType c
  => (Expr nullability' a -> Expr nullability' b -> Expr nullability' c)
  -> Expr nullability a -> Expr nullability b -> Expr nullability c
liftOpNullable f ma mb =
  boolExpr (unsafeLiftOpNullable f ma mb) (unsafeUnnullify null)
    (isNull (nullify ma) ||. isNull (nullify mb))


snull :: TypeInformation a -> Expr 'Nullable a
snull info = scastExpr info $ Expr $ Opaleye.ConstExpr Opaleye.NullLit


null :: DBType a => Expr 'Nullable a
null = snull typeInformation


nullify :: Expr nullability a -> Expr 'Nullable a
nullify (Expr a) = Expr a


seminullify :: Expr 'NonNullable a -> Expr nullability a
seminullify (Expr a) = Expr a


unsafeUnnullify :: Expr nullability' a -> Expr nullability a
unsafeUnnullify (Expr a) = Expr a


unsafeMapNullable :: ()
  => (Expr nullability' a -> Expr nullability' b)
  -> Expr nullability a -> Expr nullability b
unsafeMapNullable f ma = unsafeUnnullify (f (unsafeUnnullify ma))


unsafeLiftOpNullable :: ()
  => (Expr nullability' a -> Expr nullability' b -> Expr nullability' c)
  -> Expr nullability a -> Expr nullability b -> Expr nullability c
unsafeLiftOpNullable f ma mb =
  unsafeUnnullify (f (unsafeUnnullify ma) (unsafeUnnullify mb))
