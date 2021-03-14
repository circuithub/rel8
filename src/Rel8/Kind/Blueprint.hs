{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Kind.Blueprint
  ( Blueprint( Scalar, Vector )
  , SBlueprint( SScalar, SVector )
  , KnownBlueprint, blueprintSing
  , FromDBType, ToDBType, FromType, ToType
  , fromDBType, toDBType
  , sfromDBType, stoDBType
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- rel8
import Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  , SEmptiability( SEmptiable, SNonEmptiable )
  , KnownEmptiability, emptiabilitySing
  )
import Rel8.Kind.Nullability
  ( Nullability
  , SNullability( SNullable, SNonNullable )
  , KnownNullability, nullabilitySing
  )
import Rel8.Schema.Value ( FromValue, GetNullability, GetValue )
import Rel8.Type.Array ( Array(..) )


type Blueprint :: Type
data Blueprint = Scalar Type | Vector Emptiability Nullability Blueprint


type SBlueprint :: Blueprint -> Type
data SBlueprint blueprint where
  SScalar :: SBlueprint ('Scalar a)
  SVector :: ()
    => SEmptiability emptiability
    -> SNullability nullability
    -> SBlueprint blueprint
    -> SBlueprint ('Vector emptiability nullability blueprint)


type KnownBlueprint :: Blueprint -> Constraint
class KnownBlueprint blueprint where
  blueprintSing :: SBlueprint blueprint


instance KnownBlueprint ('Scalar a) where
  blueprintSing = SScalar


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , KnownBlueprint blueprint
  ) =>
  KnownBlueprint ('Vector emptiability nullability blueprint)
 where
  blueprintSing = SVector emptiabilitySing nullabilitySing blueprintSing


type IsList :: Type -> Bool
type family IsList a where
  IsList [_] = 'True
  IsList (NonEmpty _) = 'True
  IsList _ = 'False


type FromType' :: Bool -> Type -> Blueprint
type family FromType' isList a where
  FromType' 'False a = 'Scalar a
  FromType' 'True [a] =
    'Vector 'Emptiable (GetNullability a) (FromType (GetValue (GetNullability a) a))
  FromType' 'True (NonEmpty a) =
    'Vector 'NonEmptiable (GetNullability a) (FromType (GetValue (GetNullability a) a))


type FromType :: Type -> Blueprint
type FromType a = FromType' (IsList a) a


type FromDBType :: Type -> Blueprint
type family FromDBType a where
  FromDBType (Array emptiability nullability a) =
    'Vector emptiability nullability (FromDBType a)
  FromDBType a = 'Scalar a


type ToType :: Blueprint -> Type
type family ToType blueprint where
  ToType ('Scalar a) = a
  ToType ('Vector 'Emptiable nullability a) = [FromValue nullability (ToType a)]
  ToType ('Vector 'NonEmptiable nullability a) = NonEmpty (FromValue nullability (ToType a))


type ToDBType :: Blueprint -> Type
type family ToDBType blueprint where
  ToDBType ('Scalar a) = a
  ToDBType ('Vector emptiability nullability a) =
    Array emptiability nullability (ToDBType a)


fromDBType :: forall a dbType blueprint.
  ( blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , dbType ~ ToDBType blueprint
  , KnownBlueprint blueprint
  )
  => dbType -> a
fromDBType = sfromDBType (blueprintSing @blueprint)


sfromDBType :: (a ~ ToType blueprint, dbType ~ ToDBType blueprint)
  => SBlueprint blueprint -> dbType -> a
sfromDBType = \case
  SScalar -> id
  SVector SEmptiable SNullable blueprint ->
    \(NullableList as) -> fmap (fmap (sfromDBType blueprint)) as
  SVector SNonEmptiable SNullable blueprint ->
    \(NullableNonEmpty as) -> fmap (fmap (sfromDBType blueprint)) as
  SVector SEmptiable SNonNullable blueprint ->
    \(NonNullableList as) -> fmap (sfromDBType blueprint) as
  SVector SNonEmptiable SNonNullable blueprint ->
    \(NonNullableNonEmpty as) -> fmap (sfromDBType blueprint) as


toDBType :: forall a dbType blueprint.
  ( blueprint ~ FromType a
  , ToType blueprint ~ a
  , dbType ~ ToDBType blueprint
  , KnownBlueprint blueprint
  )
  => a -> dbType
toDBType = stoDBType (blueprintSing @blueprint)


stoDBType :: (a ~ ToType blueprint, dbType ~ ToDBType blueprint)
  => SBlueprint blueprint -> a -> dbType
stoDBType = \case
  SScalar -> id
  SVector SEmptiable SNullable blueprint ->
    NullableList . fmap (fmap (stoDBType blueprint))
  SVector SNonEmptiable SNullable blueprint ->
    NullableNonEmpty . fmap (fmap (stoDBType blueprint))
  SVector SEmptiable SNonNullable blueprint ->
    NonNullableList . fmap (stoDBType blueprint)
  SVector SNonEmptiable SNonNullable blueprint ->
    NonNullableNonEmpty . fmap (stoDBType blueprint)
