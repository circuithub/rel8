{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

-- | This module implements some machinery for implementing methods of the
-- 'Table' class for a particular special (but important) class of polymorphic
-- @Table@ types.
--
-- This special case is characterised by a @newtype@ wrapper around a bare
-- 'HTable' which is constructed by applying a type family to the polymorphic
-- type variable.
--
-- Examples of this class of @Table@ include @ListTable@ and @NonEmptyTable@.
--
-- The tricky part about implementing @Table@ for these types is 'reify' and
-- 'unreify'. There is no guarantee in general that @'Unreify' a@ is itself
-- a @Table@, let alone a @Table@ with the same 'Columns' as @a@
-- (e.g., @Unreify (AColumn Result Bool) = Bool@, and @Bool@ is not a
-- @Table@)

module Rel8.Table.Unreify
  ( Unreifiable, Unreifiability( Unreifiability ), unreifiability
  , Unreifies
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable, sReifiable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify )
import Rel8.Schema.Result ( Result )
import Rel8.Table ( Table, Context, Congruent, Unreify )


type Unreifies :: K.Context -> Type -> Constraint
type family Unreifies context a where
  Unreifies (Reify context) a = Unreifier context a
  Unreifies _ _ = ()


type Unreifiable :: Type -> Constraint
class
  ( Context a ~ Reify Aggregate => Unreifier Aggregate a
  , Context a ~ Reify Expr => Unreifier Expr a
  , Context a ~ Reify Insert => Unreifier Insert a
  , Context a ~ Reify Name => Unreifier Name a
  , Context a ~ Reify Result => Unreifier Result a
  , (forall ctx. (Context a ~ Reify (Reify ctx), Reifiable ctx) => Unreifier (Reify ctx) a)
  )
  => Unreifiable a
instance
  ( Context a ~ Reify Aggregate => Unreifier Aggregate a
  , Context a ~ Reify Expr => Unreifier Expr a
  , Context a ~ Reify Insert => Unreifier Insert a
  , Context a ~ Reify Name => Unreifier Name a
  , Context a ~ Reify Result => Unreifier Result a
  , (forall ctx. (Context a ~ Reify (Reify ctx), Reifiable ctx) => Unreifier (Reify ctx) a)
  )
  => Unreifiable a


type Unreifier :: K.Context -> Type -> Constraint
class
  ( Table (Reify context) a
  , Table context (Unreify a)
  , Congruent a (Unreify a)
  )
  => Unreifier context a
instance
  ( Table (Reify context) a
  , Table context (Unreify a)
  , Congruent a (Unreify a)
  )
  => Unreifier context a


type Unreifiability :: K.Context -> Type -> Type
data Unreifiability context a where
  Unreifiability :: Unreifier context a
    => SContext context -> Unreifiability context a


unreifiability :: (Context a ~ Reify context, Unreifiable a)
  => SContext context -> Unreifiability context a
unreifiability = \case
  SAggregate -> Unreifiability SAggregate
  SExpr -> Unreifiability SExpr
  SInsert -> Unreifiability SInsert
  SName -> Unreifiability SName
  SResult -> Unreifiability SResult
  SReify context -> case sReifiable context of
    Dict -> Unreifiability (SReify context)
