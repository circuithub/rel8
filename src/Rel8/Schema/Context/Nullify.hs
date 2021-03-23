{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
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
import Control.Applicative ( empty )
import Data.Foldable ( fold )
import Data.Kind ( Constraint )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid ( First( First ), getFirst )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate, foldInputs, mapInputs )
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Aggregate ( groupByExpr )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( nullify, unsafeUnnullify )
import Rel8.Expr.Opaleye ( unsafeFromPrimExpr, unsafeToPrimExpr )
import Rel8.Kind.Labels ( KnownLabels, labelsSing, renderLabels )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB )
  , Insertion( RequiredInsert, OptionalInsert )
  , Name( Name )
  )
import Rel8.Schema.Nullability
  ( Nullability( Nullable, NonNullable )
  , Nullabilizes
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec(..) )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid )


type Nullifiable :: Context -> Constraint
class Nullifiable context where
  encodeTag :: (DBEq db, Nullabilizes db a, KnownLabels labels)
    => Expr a
    -> context ('Spec labels 'Required db a)

  decodeTag :: (DBMonoid db, Nullabilizes db a)
    => context ('Spec labels 'Required db a)
    -> Expr a

  nullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity db a)
    -> context ('Spec labels necessity db a)
    -> context ('Spec labels necessity db (Maybe db))

  unnullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity db a)
    -> context ('Spec labels necessity db (Maybe db))
    -> context ('Spec labels necessity db a)


instance Nullifiable Aggregation where
  encodeTag = Aggregation . groupByExpr
  decodeTag (Aggregation aggregate) = fold $ undoGroupBy aggregate

  nullifier tag SSpec {nullability} (Aggregation aggregate) = Aggregation $
    mapInputs (unsafeToPrimExpr . runTag nullability tag . unsafeFromPrimExpr) $
    runTag nullability tag <$> aggregate

  unnullifier _ SSpec {nullability} (Aggregation aggregate) =
    Aggregation $ unnull nullability <$> aggregate

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable DB where
  encodeTag = DB
  decodeTag (DB a) = a
  nullifier tag SSpec {nullability} (DB a) = DB $ runTag nullability tag a
  unnullifier _ SSpec {nullability} (DB a) = DB $ unnull nullability a

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable Insertion where
  encodeTag = RequiredInsert
  decodeTag (RequiredInsert a) = a

  nullifier tag SSpec {nullability} = \case
    RequiredInsert a -> RequiredInsert $ runTag nullability tag a
    OptionalInsert ma -> OptionalInsert $ runTag nullability tag <$> ma

  unnullifier _ SSpec {nullability} = \case
    RequiredInsert a -> RequiredInsert $ unnull nullability a
    OptionalInsert ma -> OptionalInsert $ unnull nullability <$> ma

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable Name where
  encodeTag _ = nameFromLabel
  decodeTag _ = mempty
  nullifier _ _ (Name name) = Name name
  unnullifier _ _ (Name name) = Name name

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


type NullifiableEq :: Context -> Context -> Constraint
class (a ~ b, Nullifiable b) => NullifiableEq a b
instance (a ~ b, Nullifiable b) => NullifiableEq a b


runTag :: Nullability db a -> Expr Bool -> Expr a -> Expr (Maybe db)
runTag nullability tag a = case nullability of
  Nullable -> boolExpr null a tag
  NonNullable -> boolExpr null (nullify a) tag
  where
    null = Expr (Opaleye.ConstExpr Opaleye.NullLit)


unnull :: Nullability db a -> Expr (Maybe db) -> Expr a
unnull nullability a = case nullability of
  Nullable -> a
  NonNullable -> unsafeUnnullify a


-- HACK
undoGroupBy :: Aggregate (Expr _a) -> Maybe (Expr a)
undoGroupBy = getFirst . foldInputs go
  where
    go Nothing a = pure (Expr a)
    go _ _ = First empty


nameFromLabel :: forall labels necessity db a.
  KnownLabels labels => Name ('Spec labels necessity db a)
nameFromLabel = case labelsSing @labels of
  labels -> Name (NonEmpty.last (renderLabels labels))
