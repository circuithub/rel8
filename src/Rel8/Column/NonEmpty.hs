{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.NonEmpty
  ( HNonEmpty, AHNonEmpty(..)
  )
where

-- base
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Congruent, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Unreify ( Unreifiability(..), Unreifiable, unreifiability )


type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context where
  HNonEmpty (Reify context) = AHNonEmpty context
  HNonEmpty Aggregate = NonEmptyTable
  HNonEmpty Expr = NonEmptyTable
  HNonEmpty Name = NonEmptyTable
  HNonEmpty Result = NonEmpty


type AHNonEmpty :: K.Context -> Type -> Type
newtype AHNonEmpty context a = AHNonEmpty (HNonEmpty context a)


instance (Reifiable context, Unreifiable a, Table (Reify context) a) =>
  Table (Reify context) (AHNonEmpty context a)
 where
  type Context (AHNonEmpty context a) = Reify context
  type Columns (AHNonEmpty context a) = HNonEmptyTable (Columns a)
  type Unreify (AHNonEmpty context a) = HNonEmpty context (Unreify a)

  fromColumns = sfromColumnsNonEmpty contextSing
  toColumns = stoColumnsNonEmpty contextSing

  reify _ = sreifyNonEmpty (unreifiability contextSing)
  unreify _ = sunreifyNonEmpty (unreifiability contextSing)


instance
  ( Reifiable context, Reifiable context'
  , Unreifiable a, Unreifiable a'
  , Recontextualize (Reify context) (Reify context') a a'
  )
  => Recontextualize
    (Reify context)
    (Reify context')
    (AHNonEmpty context a)
    (AHNonEmpty context' a')


smapNonEmpty :: Congruent a b
  => SContext context
  -> (a -> b)
  -> (HNonEmptyTable (Columns a) (Col (Context a)) -> HNonEmptyTable (Columns b) (Col (Context b)))
  -> AHNonEmpty context a
  -> AHNonEmpty context b
smapNonEmpty = \case
  SAggregate -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
  SExpr -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
  SResult -> \f _ (AHNonEmpty as) -> AHNonEmpty (fmap f as)
  SName -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
  SReify context -> \f g (AHNonEmpty as) -> AHNonEmpty (smapNonEmpty context f g as)


sfromColumnsNonEmpty :: Table (Reify context) a
  => SContext context
  -> HNonEmptyTable (Columns a) (Col (Reify context))
  -> AHNonEmpty context a
sfromColumnsNonEmpty = \case
  SAggregate -> AHNonEmpty . NonEmptyTable
  SExpr -> AHNonEmpty . NonEmptyTable
  SResult ->
    AHNonEmpty . fmap (fromColumns . hreify) . fromColumns . hunreify
  SName -> AHNonEmpty . NonEmptyTable
  SReify context ->
    AHNonEmpty .
    smapNonEmpty context (fromColumns . hreify) hreify .
    sfromColumnsNonEmpty context .
    hunreify


stoColumnsNonEmpty :: Table (Reify context) a
  => SContext context
  -> AHNonEmpty context a
  -> HNonEmptyTable (Columns a) (Col (Reify context))
stoColumnsNonEmpty = \case
  SAggregate -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SExpr -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SResult ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHNonEmpty a) -> a)
  SName -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SReify context ->
    hreify .
    stoColumnsNonEmpty context .
    smapNonEmpty context (hunreify . toColumns) hunreify .
    (\(AHNonEmpty a) -> a)


sreifyNonEmpty :: Table (Reify context) a
  => Unreifiability context a
  -> HNonEmpty context (Unreify a)
  -> AHNonEmpty context a
sreifyNonEmpty = \case
  UResult -> AHNonEmpty . fmap (reify Refl)
  Unreifiability context ->
    smapNonEmpty context (reify Refl) hreify .
    AHNonEmpty


sunreifyNonEmpty :: Table (Reify context) a
  => Unreifiability context a
  -> AHNonEmpty context a
  -> HNonEmpty context (Unreify a)
sunreifyNonEmpty = \case
  UResult -> fmap (unreify Refl) . (\(AHNonEmpty a) -> a)
  Unreifiability context ->
    (\(AHNonEmpty a) -> a) .
    smapNonEmpty context (unreify Refl) hunreify
