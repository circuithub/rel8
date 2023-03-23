{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Null
  ( Nullify, Unnullify
  , NotNull
  , Homonullable
  , Nullity( Null, NotNull )
  , Nullable, nullable
  , Sql
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude


type IsMaybe :: Type -> Bool
type family IsMaybe a where
  IsMaybe (Maybe _) = 'True
  IsMaybe _ = 'False


type Unnullify' :: Bool -> Type -> Type
type family Unnullify' isMaybe ma where
  Unnullify' 'False a = a
  Unnullify' 'True (Maybe a) = a


type Unnullify :: Type -> Type
type Unnullify a = Unnullify' (IsMaybe a) a


type Nullify' :: Bool -> Type -> Type
type family Nullify' isMaybe a where
  Nullify' 'False a = a
  Nullify' 'True a = Maybe a


type Nullify :: Type -> Type
type Nullify a = Maybe (Unnullify a)


-- | @nullify a@ means @a@ cannot take @null@ as a value.
type NotNull :: Type -> Constraint
class (Nullable a, IsMaybe a ~ 'False) => NotNull a
instance (Nullable a, IsMaybe a ~ 'False) => NotNull a


-- | @Homonullable a b@ means that both @a@ and @b@ can be @null@, or neither
-- @a@ or @b@ can be @null@.
type Homonullable :: Type -> Type -> Constraint
class IsMaybe a ~ IsMaybe b => Homonullable a b
instance IsMaybe a ~ IsMaybe b => Homonullable a b


type Nullity :: Type -> Type
data Nullity a where
  NotNull :: NotNull a => Nullity a
  Null :: NotNull a => Nullity (Maybe a)


type Nullable' :: Bool -> Type -> Constraint
class
  ( IsMaybe a ~ isMaybe
  , IsMaybe (Unnullify a) ~ 'False
  , Nullify' isMaybe (Unnullify a) ~ a
  ) => Nullable' isMaybe a
 where
  nullable' :: Nullity a


instance IsMaybe a ~ 'False => Nullable' 'False a where
  nullable' = NotNull


instance IsMaybe a ~ 'False => Nullable' 'True (Maybe a) where
  nullable' = Null


-- | @Nullable a@ means that @rel8@ is able to check if the type @a@ is a
-- type that can take @null@ values or not.
type Nullable :: Type -> Constraint
class Nullable' (IsMaybe a) a => Nullable a
instance Nullable' (IsMaybe a) a => Nullable a


nullable :: Nullable a => Nullity a
nullable = nullable'


-- | The @Sql@ type class describes both null and not null database values,
-- constrained by a specific class.
--
-- For example, if you see @Sql DBEq a@, this means any database type that
-- supports equality, and @a@ can either be exactly an @a@, or it could also be
-- @Maybe a@.
type Sql :: (Type -> Constraint) -> Type -> Constraint
class (constraint (Unnullify a), Nullable a) => Sql constraint a
instance (constraint (Unnullify a), Nullable a) => Sql constraint a
