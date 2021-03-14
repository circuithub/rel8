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
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Aggregate ( Aggregate(..), groupByExpr )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( nullify, unsafeUnnullify )
import Rel8.Expr.Opaleye ( toPrimExpr )
import Rel8.Kind.Blueprint ( Blueprint( Scalar ) )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Kind.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB )
  , Insert( RequiredInsert, OptionalInsert )
  , Name( Name )
  , Labels( Labels )
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid )


type Nullifiable :: Context -> Constraint
class Nullifiable context where
  encodeTag :: DBEq a
    => String
    -> Expr nullability a
    -> context ('Spec 'Required nullability ('Scalar a))

  decodeTag :: DBMonoid a
    => String
    -> context ('Spec 'Required nullability ('Scalar a))
    -> Expr nullability a

  nullifier :: ()
    => String
    -> Expr 'NonNullable Bool
    -> SSpec ('Spec necessity nullability blueprint)
    -> context ('Spec necessity nullability blueprint)
    -> context ('Spec necessity 'Nullable blueprint)

  unnullifier :: ()
    => String
    -> Expr 'NonNullable Bool
    -> SSpec ('Spec necessity nullability blueprint)
    -> context ('Spec necessity 'Nullable blueprint)
    -> context ('Spec necessity nullability blueprint)


instance Nullifiable Aggregation where
  encodeTag _ = Aggregation . groupByExpr
  decodeTag _ (Aggregation aggregate) = fold $ undoGroupBy aggregate

  nullifier _ tag _ (Aggregation aggregate) = Aggregation $ case aggregate of
    Aggregate {aggregator, input, output} -> Aggregate
      { aggregator
      , input = toPrimExpr $ runTag tag (Expr input)
      , output = nullify . output
      }

  unnullifier _ _ _ (Aggregation aggregate) = Aggregation $ case aggregate of
    Aggregate {aggregator, input, output} ->
      Aggregate
        { aggregator, input
        , output = unsafeUnnullify . output
        }


instance Nullifiable DB where
  encodeTag _ = DB
  decodeTag _ (DB a) = a
  nullifier _ tag _ (DB a) = DB $ runTag tag a
  unnullifier _ tag _ (DB a) = DB $ unsafeUnnullify $ runTag tag a


instance Nullifiable Insert where
  encodeTag _ = RequiredInsert
  decodeTag _ (RequiredInsert a) = a

  nullifier _ tag _ = \case
    RequiredInsert a -> RequiredInsert $ runTag tag a
    OptionalInsert ma -> OptionalInsert $ runTag tag <$> ma

  unnullifier _ tag _ = \case
    RequiredInsert a -> RequiredInsert $ unsafeUnnullify $ runTag tag a
    OptionalInsert ma -> OptionalInsert $ unsafeUnnullify . runTag tag <$> ma


instance Nullifiable Labels where
  encodeTag tagName _ = Labels (pure tagName)
  decodeTag _ = mempty
  nullifier tableName _ _ (Labels labels) = Labels (tableName <| labels)
  unnullifier tableName _ _ = \case
    Labels (name :| (label : labels))
      | name == tableName -> Labels (label :| labels)
    Labels labels -> Labels labels


instance Nullifiable Name where
  encodeTag tagName _ = Name tagName
  decodeTag _ _ = mempty
  nullifier _ _ _ (Name name) = Name name
  unnullifier _ _ _ (Name name) = Name name


type NullifiableEq :: Context -> Context -> Constraint
class (a ~ b, Nullifiable b) => NullifiableEq a b
instance (a ~ b, Nullifiable b) => NullifiableEq a b


runTag :: Expr 'NonNullable Bool -> Expr nullability a -> Expr 'Nullable a
runTag tag a = boolExpr null (nullify a) tag
  where
    null = Expr (Opaleye.ConstExpr Opaleye.NullLit)


-- HACK
undoGroupBy :: Aggregate _nullability _a -> Maybe (Expr nullability a)
undoGroupBy Aggregate {aggregator = Nothing, input} = Just (Expr input)
undoGroupBy _ = Nothing
