{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Nullify
  ( Nullifiable( encodeTag, decodeTag, nullifier, unnullifier )
  , NullifiableEq
  )
where

-- base
import Data.Foldable ( fold )
import Data.Kind ( Constraint )
import qualified Data.List.NonEmpty as NonEmpty
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
import Rel8.Kind.Labels ( KnownLabels, labelsSing, renderLabels )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Kind.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB )
  , Insert( RequiredInsert, OptionalInsert )
  , Name( Name )
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid )


type Nullifiable :: Context -> Constraint
class Nullifiable context where
  encodeTag :: (DBEq a, KnownLabels labels)
    => Expr nullability a
    -> context ('Spec labels 'Required nullability ('Scalar a))

  decodeTag :: DBMonoid a
    => context ('Spec labels 'Required nullability ('Scalar a))
    -> Expr nullability a

  nullifier :: ()
    => Expr 'NonNullable Bool
    -> SSpec ('Spec labels necessity nullability blueprint)
    -> context ('Spec labels necessity nullability blueprint)
    -> context ('Spec labels necessity 'Nullable blueprint)

  unnullifier :: ()
    => Expr 'NonNullable Bool
    -> SSpec ('Spec labels necessity nullability blueprint)
    -> context ('Spec labels necessity 'Nullable blueprint)
    -> context ('Spec labels necessity nullability blueprint)


instance Nullifiable Aggregation where
  encodeTag = Aggregation . groupByExpr
  decodeTag (Aggregation aggregate) = fold $ undoGroupBy aggregate

  nullifier tag _ (Aggregation aggregate) = Aggregation $ case aggregate of
    Aggregate {aggregator, input, output} -> Aggregate
      { aggregator
      , input = toPrimExpr $ runTag tag (Expr input)
      , output = nullify . output
      }

  unnullifier _ _ (Aggregation aggregate) = Aggregation $ case aggregate of
    Aggregate {aggregator, input, output} ->
      Aggregate
        { aggregator, input
        , output = unsafeUnnullify . output
        }


instance Nullifiable DB where
  encodeTag = DB
  decodeTag (DB a) = a
  nullifier tag _ (DB a) = DB $ runTag tag a
  unnullifier tag _ (DB a) = DB $ unsafeUnnullify $ runTag tag a


instance Nullifiable Insert where
  encodeTag = RequiredInsert
  decodeTag (RequiredInsert a) = a

  nullifier tag _ = \case
    RequiredInsert a -> RequiredInsert $ runTag tag a
    OptionalInsert ma -> OptionalInsert $ runTag tag <$> ma

  unnullifier tag _ = \case
    RequiredInsert a -> RequiredInsert $ unsafeUnnullify $ runTag tag a
    OptionalInsert ma -> OptionalInsert $ unsafeUnnullify . runTag tag <$> ma


instance Nullifiable Name where
  encodeTag _ = nameFromLabel
  decodeTag _ = mempty
  nullifier _ _ (Name name) = Name name
  unnullifier _ _ (Name name) = Name name


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


nameFromLabel :: forall labels necessity nullability blueprint.
  KnownLabels labels => Name ('Spec labels necessity nullability blueprint)
nameFromLabel = case labelsSing @labels of
  labels -> Name (NonEmpty.last (renderLabels labels))
