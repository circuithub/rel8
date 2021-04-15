{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

{-# options -fno-warn-redundant-constraints #-}

module Rel8.Expr.Null
  ( null, snull, nullable, nullableOf
  , isNull, isNonNull
  , nullify, unsafeUnnullify
  , mapNull, liftOpNull
  , unsafeMapNull, unsafeLiftOpNull
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
import Rel8.Schema.Null ( NotNull )
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


-- | Like 'Data.Maybe.isNothing', but for @null@.
isNull :: Expr (Maybe a) -> Expr Bool
isNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNull)


-- | Like 'Data.Maybe.isJust', but for @null@.
isNonNull :: Expr (Maybe a) -> Expr Bool
isNonNull = mapPrimExpr (Opaleye.UnExpr Opaleye.OpIsNotNull)


-- | Lift an operation on non-@null@ values to an operation on possibly @null@
-- values. When given @null@, @mapNull f@ returns @null@.
-- 
-- This is like 'fmap' for 'Maybe'.
mapNull :: DBType b
  => (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
mapNull f ma = boolExpr (unsafeMapNull f ma) null (isNull ma)


-- | Lift a binary operation on non-@null@ expressions to an equivalent binary
-- operator on possibly @null@ expressions. If either of the final arguments
-- are @null@, @liftOpNull@ returns @null@.
--
-- This is like 'liftA2' for 'Maybe'.
liftOpNull :: DBType c
  => (Expr a -> Expr b -> Expr c)
  -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
liftOpNull f ma mb =
  boolExpr (unsafeLiftOpNull f ma mb) null
    (isNull ma ||. isNull mb)
{-# INLINABLE liftOpNull #-}


snull :: TypeInformation a -> Expr (Maybe a)
snull info = scastExpr info $ Expr $ Opaleye.ConstExpr Opaleye.NullLit


-- | Corresponds to SQL @null@.
null :: DBType a => Expr (Maybe a)
null = snull typeInformation


unsafeMapNull :: NotNull b
  => (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
unsafeMapNull f ma = nullify (f (unsafeUnnullify ma))


unsafeLiftOpNull :: NotNull c
  => (Expr a -> Expr b -> Expr c)
  -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
unsafeLiftOpNull f ma mb =
  nullify (f (unsafeUnnullify ma) (unsafeUnnullify mb))
