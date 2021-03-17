{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language ViewPatterns #-}

module Rel8.Expr.Null
  ( null, snull, nullable, nullableOf
  , isNull, isNonNull
  , Nullification (nullify, unsafeUnnullify)
  , mapNullable, liftOpNullable
  , unsafeMapNullable, unsafeLiftOpNullable
  )
where

-- base
import Data.Kind ( Constraint )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Bool ( boolExpr, (||.) )
import Rel8.Expr.Opaleye ( scastExpr, unsafeMapPrimExpr )
import Rel8.Kind.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Type ( DBType, TypeInformation, typeInformation )


type Nullification :: Nullability -> Nullability -> Constraint
class Nullification nonNullable nullable where
  nullify :: Expr nonNullable a -> Expr nullable a
  unsafeUnnullify :: Expr nullable a -> Expr nonNullable a


instance {-# INCOHERENT #-} Nullification 'NonNullable nullable where
  nullify (Expr a) = Expr a
  unsafeUnnullify (Expr a) = Expr a


instance {-# INCOHERENT #-} Nullification nonNullable 'Nullable where
  nullify (Expr a) = Expr a
  unsafeUnnullify (Expr a) = Expr a


nullable :: ()
  => Expr nullability b
  -> (Expr 'NonNullable a -> Expr nullability b)
  -> Expr 'Nullable a
  -> Expr nullability b
nullable b f ma = boolExpr (f (unsafeUnnullify ma)) b (isNull ma)


nullableOf :: DBType a => Maybe (Expr 'NonNullable a) -> Expr 'Nullable a
nullableOf = maybe null nullify


isNull :: Expr 'Nullable a -> Expr 'NonNullable Bool
isNull = unsafeMapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNull)


isNonNull :: Expr 'Nullable a -> Expr 'NonNullable Bool
isNonNull = unsafeMapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNotNull)


mapNullable :: (Nullification nonNullable nullable, DBType b)
  => (Expr nonNullable a -> Expr nonNullable b)
  -> Expr nullable a -> Expr nullable b
mapNullable f ma =
  boolExpr (unsafeMapNullable f ma) (unsafeUnnullify null)
    (isNull (nullify ma))


liftOpNullable :: (Nullification nonNullable nullable, DBType c)
  => (Expr nonNullable a -> Expr nonNullable b -> Expr nonNullable c)
  -> Expr nullable a -> Expr nullable b -> Expr nullable c
liftOpNullable f ma mb =
  boolExpr (unsafeLiftOpNullable f ma mb) (unsafeUnnullify null)
    (isNull (nullify ma) ||. isNull (nullify mb))


snull :: TypeInformation a -> Expr 'Nullable a
snull info = scastExpr info $ Expr $ Opaleye.ConstExpr Opaleye.NullLit


null :: DBType a => Expr 'Nullable a
null = snull typeInformation


unsafeMapNullable :: Nullification nonNullable nullable
  => (Expr nonNullable a -> Expr nonNullable b)
  -> Expr nullable a -> Expr nullable b
unsafeMapNullable f ma = nullify (f (unsafeUnnullify ma))


unsafeLiftOpNullable :: Nullification nonNullable nullable
  => (Expr nonNullable a -> Expr nonNullable b -> Expr nonNullable c)
  -> Expr nullable a -> Expr nullable b -> Expr nullable c
unsafeLiftOpNullable f ma mb =
  nullify (f (unsafeUnnullify ma) (unsafeUnnullify mb))
