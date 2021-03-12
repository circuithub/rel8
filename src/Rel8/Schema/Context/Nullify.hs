{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Nullify
  ( Nullifiable( encodeTag, decodeTag, nullifier, unnullifier )
  , NullifiableEq
  )
where

-- base
import Data.Foldable ( fold )
import Data.Kind ( Constraint )
import Data.List.NonEmpty ( NonEmpty( (:|) ), (<|) )
import Data.Typeable ( cast )
import Prelude hiding ( null, repeat, undefined, zipWith )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate(..), groupByExpr )
import Rel8.Expr.Null ( null, nullify, unsafeSemiunnullify )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNullable, SNonNullable )
  , withKnownNullability
  )
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB )
  , Insert( RequiredInsert, OptionalInsert )
  , Name( Name )
  , Labels( Labels )
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec( SSpec ) )
import Rel8.Table.Bool ( bool )
import Rel8.Type ( TypeInformation(..), withDBType )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid )


type Nullifiable :: Context -> Constraint
class Nullifiable context where
  encodeTag :: DBEq a
    => String
    -> SNullability nullability
    -> TypeInformation a
    -> Expr nullability a
    -> context ('Spec 'Required nullability a)

  decodeTag :: DBMonoid a
    => String
    -> SNullability nullability
    -> TypeInformation a
    -> context ('Spec 'Required nullability a)
    -> Expr nullability a

  nullifier :: ()
    => String
    -> Expr 'NonNullable Bool
    -> SSpec ('Spec necessity nullability a)
    -> context ('Spec necessity nullability a)
    -> context ('Spec necessity 'Nullable a)

  unnullifier :: ()
    => String
    -> Expr 'NonNullable Bool
    -> SSpec ('Spec necessity nullability a)
    -> context ('Spec necessity 'Nullable a)
    -> context ('Spec necessity nullability a)


instance Nullifiable Aggregation where
  encodeTag _ nullability _ =
    withKnownNullability nullability $ Aggregation . groupByExpr
  decodeTag _ nullability info (Aggregation aggregate) =
    fold $ undoGroupBy nullability info aggregate

  nullifier _ tag _ (Aggregation aggregate) = Aggregation $ case aggregate of
    Aggregate {operation, inType, inExpr, outExpr} -> Aggregate
      { operation
      , inNullability = SNullable
      , inType
      , inExpr = withDBType inType $ bool null (nullify inExpr) tag
      , outExpr = nullify . outExpr
      }

  unnullifier _ _ _ (Aggregation aggregate) = Aggregation $ case aggregate of
    Aggregate {operation, inNullability, inType, inExpr, outExpr} ->
      Aggregate
        { operation, inNullability, inType, inExpr
        , outExpr = unsafeSemiunnullify . outExpr
        }


instance Nullifiable DB where
  encodeTag _ _ _ = DB
  decodeTag _ _ _ (DB a) = a

  nullifier _ tag (SSpec _ _ info) (DB a) = DB $ withDBType info $
    bool null (nullify a) tag

  unnullifier _ tag (SSpec _ _ info) (DB a) = DB $ withDBType info $
    unsafeSemiunnullify $ bool null a tag


instance Nullifiable Insert where
  encodeTag _ _  _ = RequiredInsert
  decodeTag _ _ _ (RequiredInsert a) = a

  nullifier _ tag (SSpec _ _ info) = \case
    RequiredInsert a -> RequiredInsert $ withDBType info $
      bool null (nullify a) tag
    OptionalInsert ma -> OptionalInsert $ withDBType info $
      (\a -> bool null (nullify a) tag) <$> ma

  unnullifier _ tag (SSpec _ _ info) = \case
    RequiredInsert a -> RequiredInsert $ withDBType info $
      unsafeSemiunnullify $ bool null a tag
    OptionalInsert ma -> OptionalInsert $ withDBType info $
      (\a -> unsafeSemiunnullify $ bool null a tag) <$> ma


instance Nullifiable Labels where
  encodeTag tagName _ _ _ = Labels (pure tagName)
  decodeTag _ _ _ _ = mempty
  nullifier tableName _ _ (Labels labels) = Labels (tableName <| labels)
  unnullifier tableName _ _ = \case
    Labels (name :| (label : labels))
      | name == tableName -> Labels (label :| labels)
    Labels labels -> Labels labels


instance Nullifiable Name where
  encodeTag tagName _ _ _ = Name tagName
  decodeTag _ _ _ _ = mempty
  nullifier _ _ _ (Name name) = Name name
  unnullifier _ _ _ (Name name) = Name name


type NullifiableEq :: Context -> Context -> Constraint
class (a ~ b, Nullifiable b) => NullifiableEq a b
instance (a ~ b, Nullifiable b) => NullifiableEq a b


-- HACK
undoGroupBy :: ()
  => SNullability nullability
  -> TypeInformation a
  -> Aggregate _nullability _a
  -> Maybe (Expr nullability a)
undoGroupBy nullability info aggregate =
  case aggregate of
    Aggregate {operation, inNullability, inType, inExpr} ->
      case operation of
        Nothing -> case info of
          TypeInformation { typeable = Dict } -> case inType of
            TypeInformation { typeable = Dict } -> case inNullability of
              SNullable -> case nullability of
                SNullable -> cast inExpr
                SNonNullable -> Nothing
              SNonNullable -> case nullability of
                SNullable -> Nothing
                SNonNullable -> cast inExpr
        Just _ -> Nothing
