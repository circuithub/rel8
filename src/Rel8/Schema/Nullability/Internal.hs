{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Nullability.Internal
  ( Nullify, Unnullify
  , NotNull
  , Homonullable
  , Nullability( Nullable, NonNullable )
  , HasNullability, nullabilization
  , Sql, fromSql, mapSql, toSql
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Dict ( Dict( Dict ) )


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


type NotNull :: Type -> Constraint
class (HasNullability a, IsMaybe a ~ 'False) => NotNull a
instance (HasNullability a, IsMaybe a ~ 'False) => NotNull a
instance {-# OVERLAPPING #-} NotNull Opaque


type Homonullable :: Type -> Type -> Constraint
class IsMaybe a ~ IsMaybe b => Homonullable a b
instance IsMaybe a ~ IsMaybe b => Homonullable a b
instance {-# OVERLAPPING #-} Homonullable Opaque Opaque


type Nullability :: Type -> Type
data Nullability a where
  NonNullable :: NotNull a => Nullability a
  Nullable :: NotNull a => Nullability (Maybe a)


type HasNullability' :: Bool -> Type -> Constraint
class
  ( IsMaybe a ~ isMaybe
  , IsMaybe (Unnullify a) ~ 'False
  , Nullify' isMaybe (Unnullify a) ~ a
  ) => HasNullability' isMaybe a
 where
  nullabilization' :: Nullability a


instance IsMaybe a ~ 'False => HasNullability' 'False a where
  nullabilization' = NonNullable


instance IsMaybe a ~ 'False => HasNullability' 'True (Maybe a) where
  nullabilization' = Nullable


type HasNullability :: Type -> Constraint
class HasNullability' (IsMaybe a) a => HasNullability a
instance HasNullability' (IsMaybe a) a => HasNullability a
instance {-# OVERLAPPING #-} HasNullability Opaque


nullabilization :: HasNullability a => Nullability a
nullabilization = nullabilization'


-- | The @Sql@ type class describes both null and not null database values,
-- constrained by a specific class.
--
-- For example, if you see @Sql DBEq a@, this means any database type that
-- supports equality, and @a@ can either be exactly an @a@, or it could also be
-- @Maybe a@.
type Sql :: (Type -> Constraint) -> Type -> Constraint
class
  ( (forall c. (forall x. (constraint x => c x)) => Sql c a)
  , HasNullability a
  , constraint (Unnullify a)
  )
  => Sql constraint a
instance (constraint (Unnullify a), HasNullability a) => Sql constraint a


fromSql :: Dict (Sql constraint) a -> (Nullability a, Dict constraint (Unnullify a))
fromSql Dict = (nullabilization, Dict)


mapSql :: ()
  => (forall x. Dict constraint x -> Dict constraint' x)
  -> Dict (Sql constraint) a -> Dict (Sql constraint') a
mapSql f dict = case fromSql dict of
  (nullability, dict') -> toSql nullability (f dict')


toSql :: Nullability a -> Dict constraint (Unnullify a) -> Dict (Sql constraint) a
toSql NonNullable Dict = Dict
toSql Nullable Dict = Dict
