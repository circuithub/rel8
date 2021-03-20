{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Kind.Blueprint
  ( Blueprint( Scalar, Vector )
  , SBlueprint( SScalar, SVector )
  , KnownBlueprint, blueprintSing
  , IsList, IsArray
  , FromDBType, ToDBType, FromType, ToType
  , fromDBType, toDBType
  , sfromDBType, stoDBType
  , blueprintRoundtripsViaType, typeRoundtripsViaBlueprint
  , simplifyTypeBlueprint
  , blueprintRoundtripsViaDBType, dbTypeRoundtripsViaBlueprint
  , simplifyDBTypeBlueprint
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Type.Equality ( (:~:)( Refl ) )
import GHC.TypeLits ( TypeError, ErrorMessage( Text ) )
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

-- rel8
import Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  , SEmptiability( SEmptiable, SNonEmptiable )
  , KnownEmptiability, emptiabilitySing
  )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability, nullabilitySing
  )
import Rel8.Schema.Value
  ( FromValue, GetNullability, GetValue
  , nullableMaybeLemma
  )
import Rel8.Type.Array ( Array(..) )
import Rel8.Type.Information ( TypeInformation )
import Rel8.Type.Scalar ( DBScalar, scalarInformation )


type Blueprint :: Type
data Blueprint = Scalar Type | Vector Emptiability Nullability Blueprint


type SBlueprint :: Blueprint -> Type
data SBlueprint blueprint where
  SScalar ::
    ( IsArray a ~ 'False
    , IsList a ~ 'False
    , GetNullability a ~ 'NonNullable
    )
    => TypeInformation a
    -> SBlueprint ('Scalar a)
  SVector :: ()
    => SEmptiability emptiability
    -> SNullability nullability
    -> SBlueprint blueprint
    -> SBlueprint ('Vector emptiability nullability blueprint)


type KnownBlueprint :: Blueprint -> Constraint
class KnownBlueprint blueprint where
  blueprintSing :: SBlueprint blueprint


instance
  ( IsArray a ~ 'False
  , IsList a ~ 'False
  , GetNullability a ~ 'NonNullable
  , DBScalar a
  ) => KnownBlueprint ('Scalar a)
 where
  blueprintSing = SScalar scalarInformation


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


type IsArray :: Type -> Bool
type family IsArray a where
  IsArray (Array _ _ _) = 'True
  IsArray _ = 'False


type FromType' :: Bool -> Type -> Blueprint
type family FromType' isList a where
  FromType' 'False a = 'Scalar a
  FromType' 'True [a] =
    'Vector 'Emptiable (GetNullability a) (FromType (GetValue (GetNullability a) a))
  FromType' 'True (NonEmpty a) =
    'Vector 'NonEmptiable (GetNullability a) (FromType (GetValue (GetNullability a) a))


type FromType :: Type -> Blueprint
type FromType a = FromType' (IsList a) a


type FromDBType' :: Bool -> Type -> Blueprint
type family FromDBType' isArray a where
  FromDBType' 'False a = 'Scalar a
  FromDBType' 'True (Array emptiability nullability a) =
    'Vector emptiability nullability (FromDBType a)


type FromDBType :: Type -> Blueprint
type FromDBType a = FromDBType' (IsArray a) a


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


type GuardScalar :: Nullability -> Type -> Type
type family GuardScalar nullability scalar where
  GuardScalar 'Nullable _ = TypeError ('Text "Maybe is not a valid Scalar")
  GuardScalar 'NonNullable a = a


fromDBType :: forall a dbType blueprint.
  ( blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , dbType ~ ToDBType blueprint
  , KnownBlueprint blueprint
  )
  => dbType -> a
fromDBType = sfromDBType (blueprintSing @blueprint)


toDBType :: forall a dbType blueprint.
  ( blueprint ~ FromType a
  , ToType blueprint ~ a
  , dbType ~ ToDBType blueprint
  , KnownBlueprint blueprint
  )
  => a -> dbType
toDBType = stoDBType (blueprintSing @blueprint)


sfromDBType :: (a ~ ToType blueprint, dbType ~ ToDBType blueprint)
  => SBlueprint blueprint -> dbType -> a
sfromDBType = \case
  SScalar _ -> id
  SVector SEmptiable SNullable blueprint ->
    \(NullableList as) -> fmap (fmap (sfromDBType blueprint)) as
  SVector SNonEmptiable SNullable blueprint ->
    \(NullableNonEmpty as) -> fmap (fmap (sfromDBType blueprint)) as
  SVector SEmptiable SNonNullable blueprint ->
    \(NonNullableList as) -> fmap (sfromDBType blueprint) as
  SVector SNonEmptiable SNonNullable blueprint ->
    \(NonNullableNonEmpty as) -> fmap (sfromDBType blueprint) as


stoDBType :: (a ~ ToType blueprint, dbType ~ ToDBType blueprint)
  => SBlueprint blueprint -> a -> dbType
stoDBType = \case
  SScalar _ -> id
  SVector SEmptiable SNullable blueprint ->
    NullableList . fmap (fmap (stoDBType blueprint))
  SVector SNonEmptiable SNullable blueprint ->
    NullableNonEmpty . fmap (fmap (stoDBType blueprint))
  SVector SEmptiable SNonNullable blueprint ->
    NonNullableList . fmap (stoDBType blueprint)
  SVector SNonEmptiable SNonNullable blueprint ->
    NonNullableNonEmpty . fmap (stoDBType blueprint)


typeIsNeverMaybe :: ()
  => SBlueprint blueprint
  -> GetNullability (ToType blueprint) :~: 'NonNullable
typeIsNeverMaybe = \case
  SScalar _ -> Refl
  SVector emptiability _ _ -> case emptiability of
    SEmptiable -> Refl
    SNonEmptiable -> Refl


typeRoundtripsViaBlueprint :: ()
  => SBlueprint blueprint
  -> FromType (ToType blueprint) :~: blueprint
typeRoundtripsViaBlueprint = \case
  SScalar _ -> Refl
  SVector emptiability nullability blueprint ->
    case typeRoundtripsViaBlueprint blueprint of
      Refl -> case typeIsNeverMaybe blueprint of
        Refl -> case emptiability of
          SEmptiable -> case nullability of
            SNullable -> Refl
            SNonNullable -> Refl
          SNonEmptiable -> case nullability of
            SNullable -> Refl
            SNonNullable -> Refl


blueprintRoundtripsViaType :: forall a. ()
  => SBlueprint (FromType a)
  -> ToType (FromType a) :~: a
blueprintRoundtripsViaType = \case
  SScalar _ -> case fromTypeScalar @a of
    Refl -> case fromType'Scalar @a of
      Refl -> Refl
  SVector emptiability nullability blueprint ->
    case fromTypeVector @a of
      Refl -> case emptiability of
        SEmptiable -> fromType'EmptiableVector $
          \(Refl :: a :~: [x]) _ _ ->
            case nullability of
              SNullable -> case nullableMaybeLemma @x of
                (Refl :: x :~: Maybe b) ->
                  case blueprintRoundtripsViaType @b blueprint of
                    Refl -> Refl
              SNonNullable ->
                case blueprintRoundtripsViaType @x blueprint of
                  Refl -> Refl
        SNonEmptiable -> fromType'NonEmptiableVector $
          \(Refl :: a :~: NonEmpty x) _ _ ->
            case nullability of
              SNullable -> case nullableMaybeLemma @x of
                (Refl :: x :~: Maybe b) ->
                  case blueprintRoundtripsViaType @b blueprint of
                    Refl -> Refl
              SNonNullable ->
                case blueprintRoundtripsViaType @x blueprint of
                  Refl -> Refl


simplifyTypeBlueprint :: forall blueprint. ()
  => SBlueprint (FromType (ToType blueprint))
  -> SBlueprint blueprint
simplifyTypeBlueprint = \case
  SScalar info -> case fromTypeScalar @(ToType blueprint) of
    Refl -> case fromType'Scalar @(ToType blueprint) of
      Refl -> case toTypeScalar @blueprint of
        Refl -> SScalar info
  SVector emptiability nullability (blueprint :: SBlueprint blueprint1) ->
    case fromTypeVector @(ToType blueprint) of
      Refl -> case emptiability of
        SEmptiable ->
          fromType'EmptiableVector @(ToType blueprint) $ \(Refl :: ToType blueprint :~: [a]) _ _ ->
            toTypeEmptiableVector @blueprint $
              \(Refl :: blueprint :~: 'Vector 'Emptiable nullability1 blueprint') _ ->
                case nullability of
                  SNonNullable -> case typeIsNeverMaybe blueprint of
                    Refl -> case fromValueNonNullable @nullability1 @(ToType blueprint') of
                      (_, Refl) -> SVector emptiability nullability $
                        simplifyTypeBlueprint blueprint
                  SNullable -> case typeIsNeverMaybe blueprint of
                    Refl -> case blueprintRoundtripsViaType @(GetValue (GetNullability a) a) blueprint of
                      Refl -> case nullableMaybeLemma @a of
                        Refl -> case fromValueNullable @nullability1 @(ToType blueprint') of
                          (_, Refl) -> SVector emptiability nullability $ simplifyTypeBlueprint blueprint
        SNonEmptiable ->
          fromType'NonEmptiableVector @(ToType blueprint) $ \(Refl :: ToType blueprint :~: NonEmpty a) _ _ ->
            toTypeNonEmptiableVector @blueprint $
              \(Refl :: blueprint :~: 'Vector 'NonEmptiable nullability1 blueprint') _ ->
                case nullability of
                  SNonNullable -> case typeIsNeverMaybe blueprint of
                    Refl -> case fromValueNonNullable @nullability1 @(ToType blueprint') of
                      (_, Refl) -> SVector emptiability nullability $
                        simplifyTypeBlueprint blueprint
                  SNullable -> case typeIsNeverMaybe blueprint of
                    Refl -> case blueprintRoundtripsViaType @(GetValue (GetNullability a) a) blueprint of
                      Refl -> case nullableMaybeLemma @a of
                        Refl -> case fromValueNullable @nullability1 @(ToType blueprint') of
                          (_, Refl) -> SVector emptiability nullability $ simplifyTypeBlueprint blueprint


dbTypeRoundtripsViaBlueprint :: ()
  => SBlueprint blueprint
  -> FromDBType (ToDBType blueprint) :~: blueprint
dbTypeRoundtripsViaBlueprint = \case
  SScalar _ -> Refl
  SVector _ _ blueprint -> case dbTypeRoundtripsViaBlueprint blueprint of
    Refl -> Refl


blueprintRoundtripsViaDBType :: forall a. ()
  => SBlueprint (FromDBType a)
  -> ToDBType (FromDBType a) :~: a
blueprintRoundtripsViaDBType = \case
  SScalar _ -> case fromDBTypeScalar @a of
    Refl -> case fromDBType'Scalar @a of
      Refl -> Refl
  SVector _ _ blueprint ->
    case fromDBTypeVector @a of
      Refl -> fromDBType'Vector @a $
        \(Refl :: a :~: Array emptiability nullability x) Refl ->
        case blueprintRoundtripsViaDBType @x blueprint of
          Refl -> Refl


simplifyDBTypeBlueprint :: forall blueprint. ()
  => SBlueprint (FromDBType (ToDBType blueprint))
  -> SBlueprint blueprint
simplifyDBTypeBlueprint = \case
  SScalar info -> case fromDBTypeScalar @(ToDBType blueprint) of
    Refl -> case fromDBType'Scalar @(ToDBType blueprint) of
      Refl -> case toDBTypeScalar @blueprint of
        Refl -> SScalar info
  SVector emptiability nullability blueprint ->
    case fromDBTypeVector @(ToDBType blueprint) of
      Refl -> fromDBType'Vector @(ToDBType blueprint) $ \Refl Refl ->
        toDBTypeVector @blueprint $ \Refl Refl ->
          SVector emptiability nullability $
            simplifyDBTypeBlueprint blueprint


fromTypeScalar :: FromType x ~ 'Scalar a
  => IsList x :~: 'False
fromTypeScalar = unsafeCoerce Refl


fromTypeVector :: FromType x ~ 'Vector emptiability nullability a
  => IsList x :~: 'True
fromTypeVector = unsafeCoerce Refl


fromType'Scalar :: FromType' 'False x ~ 'Scalar a => x :~: a
fromType'Scalar = Refl


fromType'EmptiableVector :: ()
  => FromType' 'True x ~ 'Vector 'Emptiable nullability blueprint
  => (forall a. ()
    => x :~: [a]
    -> nullability :~: GetNullability a
    -> blueprint :~: FromType (GetValue (GetNullability a) a)
    -> b)
  -> b
fromType'EmptiableVector f =
  f (unsafeCoerce Refl) (unsafeCoerce Refl) (unsafeCoerce Refl)


fromType'NonEmptiableVector :: ()
  => FromType' 'True x ~ 'Vector 'NonEmptiable nullability blueprint
  => (forall a. ()
    => x :~: NonEmpty a
    -> nullability :~: GetNullability a
    -> blueprint :~: FromType (GetValue (GetNullability a) a)
    -> b)
  -> b
fromType'NonEmptiableVector f =
  f (unsafeCoerce Refl) (unsafeCoerce Refl) (unsafeCoerce Refl)


fromDBTypeScalar :: FromDBType x ~ 'Scalar a
  => IsArray x :~: 'False
fromDBTypeScalar = unsafeCoerce Refl


fromDBTypeVector :: FromDBType x ~ 'Vector emptiability nullability a
  => IsArray x :~: 'True
fromDBTypeVector = unsafeCoerce Refl


fromDBType'Scalar :: FromDBType' 'False x ~ 'Scalar a => x :~: a
fromDBType'Scalar = Refl


fromDBType'Vector :: ()
  => FromDBType' 'True x ~ 'Vector emptiability nullability blueprint
  => (forall a. ()
    => x :~: Array emptiability nullability a
    -> blueprint :~: FromDBType a
    -> b)
  -> b
fromDBType'Vector f = f (unsafeCoerce Refl) (unsafeCoerce Refl)


toTypeScalar :: (ToType blueprint ~ a, IsList a ~ 'False)
  => blueprint :~: 'Scalar a
toTypeScalar = unsafeCoerce Refl


toTypeEmptiableVector :: forall blueprint a b. ToType blueprint ~ [a]
  => (forall nullability blueprint'. ()
    => blueprint :~: 'Vector 'Emptiable nullability blueprint'
    -> a :~: FromValue nullability (ToType blueprint')
    -> b)
  -> b
toTypeEmptiableVector f = f (unsafeCoerce Refl) (unsafeCoerce Refl)


toTypeNonEmptiableVector :: ToType blueprint ~ NonEmpty a
  => (forall nullability blueprint'. ()
    => blueprint :~: 'Vector 'NonEmptiable nullability blueprint'
    -> a :~: FromValue nullability (ToType blueprint')
    -> b)
  -> b
toTypeNonEmptiableVector f = f (unsafeCoerce Refl) (unsafeCoerce Refl)


toDBTypeScalar :: (ToDBType blueprint ~ a, IsArray a ~ 'False)
  => blueprint :~: 'Scalar a
toDBTypeScalar = unsafeCoerce Refl


toDBTypeVector ::
  ( ToDBType blueprint ~ Array emptiability nullability a
  )
  => (forall blueprint'. ()
    => blueprint :~: 'Vector emptiability nullability blueprint'
    -> a :~: ToDBType blueprint'
    -> b)
  -> b
toDBTypeVector f = f (unsafeCoerce Refl) (unsafeCoerce Refl)


fromValueNullable ::
  ( FromValue nullability a ~ Maybe x
  , GetNullability x ~ 'NonNullable
  )
  => (x :~: a, nullability :~: 'Nullable)
fromValueNullable = (unsafeCoerce Refl, unsafeCoerce Refl)


fromValueNonNullable ::
  ( FromValue nullability a ~ x
  , GetNullability x ~ 'NonNullable
  )
  => (x :~: a, nullability :~: 'NonNullable)
fromValueNonNullable = (unsafeCoerce Refl, unsafeCoerce Refl)
