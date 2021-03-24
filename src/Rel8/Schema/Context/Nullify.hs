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
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Kind.Labels ( KnownLabels, labelsSing, renderLabels )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.Context
  ( Interpretation
  , Col, Col'(..)
  , Insertion
  , Name( Name )
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability ( Nullability( Nullable, NonNullable ), Sql )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid )


type Nullifiable :: K.Context -> Constraint
class Interpretation context => Nullifiable context where
  encodeTag :: (Sql DBEq a, KnownLabels labels)
    => Expr a
    -> Col context ('Spec labels 'Required db a)

  decodeTag :: Sql DBMonoid a
    => Col context ('Spec labels 'Required db a)
    -> Expr a

  nullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity db a)
    -> Col context ('Spec labels necessity db a)
    -> Col context ('Spec labels necessity db (Maybe db))

  unnullifier :: ()
    => Expr Bool
    -> SSpec ('Spec labels necessity db a)
    -> Col context ('Spec labels necessity db (Maybe db))
    -> Col context ('Spec labels necessity db a)


instance Nullifiable Aggregate where
  encodeTag = Aggregation . groupByExpr
  decodeTag (Aggregation aggregate) = fold $ undoGroupBy aggregate

  nullifier tag SSpec {nullability} (Aggregation aggregate) = Aggregation $
    mapInputs (toPrimExpr . runTag nullability tag . fromPrimExpr) $
    runTag nullability tag <$> aggregate

  unnullifier _ SSpec {nullability} (Aggregation aggregate) =
    Aggregation $ unnull nullability <$> aggregate

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


instance Nullifiable Expr where
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
  nullifier _ _ (Col (Name name)) = Col (Name name)
  unnullifier _ _ (Col (Name name)) = Col (Name name)

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


type NullifiableEq :: K.Context -> K.Context -> Constraint
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
  KnownLabels labels => Col Name ('Spec labels necessity db a)
nameFromLabel = case labelsSing @labels of
  labels -> Col (Name (NonEmpty.last (renderLabels labels)))
