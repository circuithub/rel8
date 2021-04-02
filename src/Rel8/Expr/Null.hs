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
import Rel8.Schema.Nullability ( NotNull )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation )


-- | Lift an expression that can't be @null@ to a type that might be @null@.
-- This is an identity operation in terms of any generated query, and just
-- modifies the query's type.
nullify :: NotNull a => Expr a -> Expr (Maybe a)
nullify (Expr a) = Expr a


unsafeUnnullify :: Expr (Maybe a) -> Expr a
unsafeUnnullify (Expr a) = Expr a


-- | Like 'maybe', but to eliminate @null@.
nullable :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
nullable b f ma = boolExpr (f (unsafeUnnullify ma)) b (isNull ma)


nullableOf :: DBType a => Maybe (Expr a) -> Expr (Maybe a)
nullableOf = maybe null nullify


-- | Like 'isNothing', but for @null@.
isNull :: Expr (Maybe a) -> Expr Bool
isNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNull)


-- | Like 'isJust', but for @null@.
isNonNull :: Expr (Maybe a) -> Expr Bool
isNonNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNotNull)


-- | Lift an operation on non-@null@ values to an operation on possibly @null@
-- values. When given @null@, @mapNullable f@ returns @null@.
-- 
-- This is like 'fmap' for 'Maybe'.
mapNullable :: DBType b
  => (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
mapNullable f ma = boolExpr (unsafeMapNullable f ma) null (isNull ma)


-- | Lift a binary operation on non-@null@ expressions to an equivalent binary
-- operator on possibly @null@ expressions. If either of the final arguments
-- are @null@, @liftOpNullable@ returns @null@.
--
-- This is like 'liftA2' for 'Maybe'.
liftOpNullable :: DBType c
  => (Expr a -> Expr b -> Expr c)
  -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
liftOpNullable f ma mb =
  boolExpr (unsafeLiftOpNullable f ma mb) null
    (isNull ma ||. isNull mb)


snull :: TypeInformation a -> Expr (Maybe a)
snull info = scastExpr info $ Expr $ Opaleye.ConstExpr Opaleye.NullLit


-- | Corresponds to SQL @null@.
null :: DBType a => Expr (Maybe a)
null = snull typeInformation


unsafeMapNullable :: NotNull b
  => (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
unsafeMapNullable f ma = nullify (f (unsafeUnnullify ma))


unsafeLiftOpNullable :: NotNull c
  => (Expr a -> Expr b -> Expr c)
  -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
unsafeLiftOpNullable f ma mb =
  nullify (f (unsafeUnnullify ma) (unsafeUnnullify mb))
