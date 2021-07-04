{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.NonEmpty
  ( HNonEmpty, AHNonEmpty(..)
  )
where

-- base
import Control.Category ( id )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Type.Equality ( (:~:)( Refl ), apply )
import Prelude hiding ( id )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Abstract ( exclusivity, virtualOrResult )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result, absurd )
import Rel8.Table
  ( Table, Columns, Congruent, Context, fromColumns, toColumns
  , Unreify, reify, unreify, coherence, congruence
  )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )
import Rel8.Table.Recontextualize ( Recontextualize )


-- | Nest a 'NonEmpty' list within a 'Rel8able'. @HNonEmpty f a@ will produce a
-- 'NonEmptyTable' @a@ in the 'Expr' context, and a 'NonEmpty' @a@ in the
-- 'Result' context.
type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context = nonEmpty | nonEmpty -> context where
  HNonEmpty (Reify context) = AHNonEmpty context
  HNonEmpty Aggregate = NonEmptyTable Aggregate
  HNonEmpty Expr = NonEmptyTable Expr
  HNonEmpty Name = NonEmptyTable Name
  HNonEmpty Result = NonEmpty


type AHNonEmpty :: K.Context -> Type -> Type
newtype AHNonEmpty context a = AHNonEmpty (HNonEmpty context a)


instance (Reifiable context, Table (Reify context) a) =>
  Table (Reify context) (AHNonEmpty context a)
 where
  type Context (AHNonEmpty context a) = Reify context
  type Columns (AHNonEmpty context a) = HNonEmptyTable (Columns a)
  type Unreify (AHNonEmpty context a) = HNonEmpty context (Unreify a)

  fromColumns = sfromColumnsNonEmpty contextSing
  toColumns = stoColumnsNonEmpty contextSing

  reify _ = sreifyNonEmpty contextSing
  unreify _ = sunreifyNonEmpty contextSing

  coherence = case contextSing @context of
    SAggregate -> coherence @(Reify context) @a
    SExpr -> coherence @(Reify context) @a
    SName -> coherence @(Reify context) @a
    SResult -> \Refl -> absurd
    SReify _ -> \Refl _ -> Refl

  congruence proof@Refl abstract = case contextSing @context of
    SAggregate -> id `apply` congruence @(Reify context) @a proof abstract
    SExpr -> id `apply` congruence @(Reify context) @a proof abstract
    SName -> id `apply` congruence @(Reify context) @a proof abstract
    SResult -> absurd abstract
    SReify _ -> id `apply` congruence @(Reify context) @a proof abstract


instance
  ( Reifiable context, Reifiable context'
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


sreifyNonEmpty :: forall context a. Table (Reify context) a
  => SContext context
  -> HNonEmpty context (Unreify a)
  -> AHNonEmpty context a
sreifyNonEmpty context = case virtualOrResult context of
  Left Refl -> AHNonEmpty . fmap (reify Refl)
  Right virtual ->
    case coherence @(Reify context) @a Refl abstract of
      Refl -> case congruence @(Reify context) @a Refl abstract of
        Refl ->
          smapNonEmpty context (reify Refl) hreify .
          AHNonEmpty
    where
      abstract = exclusivity virtual


sunreifyNonEmpty :: forall context a. Table (Reify context) a
  => SContext context
  -> AHNonEmpty context a
  -> HNonEmpty context (Unreify a)
sunreifyNonEmpty context = case virtualOrResult context of
  Left Refl -> fmap (unreify Refl) . (\(AHNonEmpty a) -> a)
  Right virtual ->
    case coherence @(Reify context) @a Refl abstract of
      Refl -> case congruence @(Reify context) @a Refl abstract of
        Refl ->
          (\(AHNonEmpty a) -> a) .
          smapNonEmpty context (unreify Refl) hunreify
    where
      abstract = exclusivity virtual
