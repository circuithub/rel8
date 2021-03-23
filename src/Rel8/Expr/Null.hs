{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

{-# options -fno-warn-redundant-constraints #-}

module Rel8.Expr.Null
  ( null, snull, nullable, nullableOf
  , isNull, isNonNull
  , nullify, unsafeUnnullify
  , mapNullable, liftOpNullable
  , unsafeMapNullable, unsafeLiftOpNullable
  )
where

-- base
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Bool ( (||.), boolExpr )
import Rel8.Expr.Opaleye ( scastExpr, mapPrimExpr )
import Rel8.Schema.Nullability ( Nullability( Nullable ), IsMaybe )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation )


nullify :: IsMaybe a ~ 'False => Expr a -> Expr (Maybe a)
nullify (Expr a) = Expr a


unsafeUnnullify :: Expr (Maybe a) -> Expr a
unsafeUnnullify (Expr a) = Expr a


nullable :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
nullable b f ma = boolExpr (f (unsafeUnnullify ma)) b (isNull ma)


nullableOf :: (IsMaybe a ~ 'False, DBType a)
  => Maybe (Expr a) -> Expr (Maybe a)
nullableOf = maybe null nullify


isNull :: Expr (Maybe a) -> Expr Bool
isNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNull)


isNonNull :: Expr (Maybe a) -> Expr Bool
isNonNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNotNull)


mapNullable :: (IsMaybe b ~ 'False, DBType b)
  => (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
mapNullable f ma = boolExpr (unsafeMapNullable f ma) null (isNull ma)


liftOpNullable :: (IsMaybe c ~ 'False, DBType c)
  => (Expr a -> Expr b -> Expr c)
  -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
liftOpNullable f ma mb =
  boolExpr (unsafeLiftOpNullable f ma mb) null
    (isNull ma ||. isNull mb)


snull :: Nullability a (Maybe a) -> TypeInformation a -> Expr (Maybe a)
snull nullability info =
  scastExpr nullability info $ Expr $ Opaleye.ConstExpr Opaleye.NullLit


null :: (IsMaybe a ~ 'False, DBType a) => Expr (Maybe a)
null = snull Nullable typeInformation


unsafeMapNullable :: IsMaybe b ~ 'False
  => (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
unsafeMapNullable f ma = nullify (f (unsafeUnnullify ma))


unsafeLiftOpNullable :: IsMaybe c ~ 'False
  => (Expr a -> Expr b -> Expr c)
  -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
unsafeLiftOpNullable f ma mb =
  nullify (f (unsafeUnnullify ma) (unsafeUnnullify mb))
