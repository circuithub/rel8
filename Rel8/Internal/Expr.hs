{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Internal.Expr where

import Data.Proxy (Proxy(..))
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

--------------------------------------------------------------------------------
-- | Database-side PostgreSQL expressions of a given type.

newtype Expr t = Expr O.PrimExpr

type role Expr representational


--------------------------------------------------------------------------------
-- | (Unsafely) coerce the phantom type given to 'Expr'. This operation is
-- not witnessed by the database at all, so use with care! For example,
-- @unsafeCoerceExpr :: Expr Int -> Expr Text@ /will/ end up with an exception
-- when you finally try and run a query!
unsafeCoerceExpr :: forall b a. Expr a -> Expr b
unsafeCoerceExpr (Expr a) = Expr a


--------------------------------------------------------------------------------
-- | Use a cast operation in the database layer to convert between Expr types.
-- This is unsafe as it is possible to introduce casts that cannot be performed
-- by PostgreSQL. For example,
-- @unsafeCastExpr "timestamptz" :: Expr Bool -> Expr UTCTime@ makes no sense.
unsafeCastExpr :: forall b a. String -> Expr a -> Expr b
unsafeCastExpr t = columnToExpr . O.unsafeCast t . exprToColumn


--------------------------------------------------------------------------------
-- | Lift an 'Expr' to be nullable. Like the 'Just' constructor.
--
-- If an Expr is already nullable, then this acts like the identity function.
-- This is useful as it allows projecting an already-nullable column from a left
-- join.
class ToNullable a maybeA | a -> maybeA where
  toNullable :: Expr a -> Expr maybeA

instance ToNullableHelper a maybeA (IsMaybe a) => ToNullable a maybeA where
  toNullable = toNullableHelper (Proxy @(IsMaybe a))

--------------------------------------------------------------------------------
-- | A helper class to implement 'ToNullable' by scrutenising the argument
-- and partioning into 'Maybe'/'NotMaybe' while retaining functional
-- dependencies.
class isMaybe ~ IsMaybe a =>
        ToNullableHelper a maybeA isMaybe | isMaybe a -> maybeA where
  toNullableHelper :: proxy join -> Expr a -> Expr maybeA

instance IsMaybe a ~ 'False => ToNullableHelper a (Maybe a) 'False where
  toNullableHelper _ = unsafeCoerceExpr @(Maybe a)

instance ToNullableHelper (Maybe a) (Maybe a) 'True where
  toNullableHelper _ = id


--------------------------------------------------------------------------------
type family IsMaybe a :: Bool where
  IsMaybe (Maybe a) = 'True
  IsMaybe _ = 'False


--------------------------------------------------------------------------------
-- | Convert an 'Expr' into an @opaleye@ 'O.Column'. Does not preserve the
-- phantom type.
exprToColumn :: Expr a -> O.Column b
exprToColumn (Expr a) = O.Column a


--------------------------------------------------------------------------------
-- | Convert an @opaleye 'O.Column' into an 'Expr'. Does not preserve the
-- phantom type.
columnToExpr :: O.Column a -> Expr b
columnToExpr (O.Column a) = Expr a
