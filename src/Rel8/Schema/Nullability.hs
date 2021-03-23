{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Nullability
  ( IsMaybe, Nullify, Unnullify
  , Nullability( Nullable, NonNullable )
  , Sql
  , Nullabilizes, nullabilization
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Opaque ( Opaque )


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


type Nullability :: Type -> Type -> Type
data Nullability a ma where
  NonNullable :: IsMaybe a ~ 'False => Nullability a a
  Nullable :: IsMaybe a ~ 'False => Nullability a (Maybe a)


type Nullabilizes' :: Bool -> Type -> Type -> Constraint
class
  ( IsMaybe ma ~ isMaybe
  , IsMaybe a ~ 'False
  , Unnullify ma ~ a
  , Nullify' isMaybe a ~ ma
  ) => Nullabilizes' isMaybe a ma | isMaybe ma -> a, isMaybe a -> ma
 where
  nullabilization' :: Nullability a ma


instance IsMaybe a ~ 'False => Nullabilizes' 'False a a where
  nullabilization' = NonNullable


instance IsMaybe a ~ 'False => Nullabilizes' 'True a (Maybe a) where
  nullabilization' = Nullable


type Nullabilizes :: Type -> Type -> Constraint
class Nullabilizes' (IsMaybe ma) a ma => Nullabilizes a ma
instance Nullabilizes' (IsMaybe ma) a ma => Nullabilizes a ma
instance {-# OVERLAPPING #-} Nullabilizes Opaque Opaque


type Sql :: (Type -> Constraint) -> Type -> Constraint
class
  ( (forall c. (forall x. (constraint x => c x)) => Sql c a)
  , Nullabilizes (Unnullify a) a
  , constraint (Unnullify a)
  )
  => Sql constraint a
instance (constraint db, Nullabilizes db a) => Sql constraint a
instance {-# OVERLAPPING #-} (constraint Opaque, Sql constraint Opaque) => Sql constraint Opaque
-- instance {-# OVERLAPPING #-} Sql OpaqueC Opaque => Sql OpaqueC Opaque


nullabilization :: forall a db. Nullabilizes db a => Nullability db a
nullabilization = nullabilization'
