{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
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
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.Insert ( Insert )
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
import Rel8.Table.Unreify ( Unreifiable )


type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context where
  HNonEmpty (Reify context) = AHNonEmpty context
  HNonEmpty Aggregate = NonEmptyTable
  HNonEmpty Expr = NonEmptyTable
  HNonEmpty Insert = NonEmptyTable
  HNonEmpty Name = NonEmptyTable
  HNonEmpty Result = NonEmpty


type AHNonEmpty :: K.Context -> Type -> Type
newtype AHNonEmpty context a = AHNonEmpty (HNonEmpty context a)


instance
  ( Reifiable context
  , Table (Reify context) a
  , Unreifiable (Reify context) a
  )
  => Table (Reify context) (AHNonEmpty context a)
 where
  type Context (AHNonEmpty context a) = Reify context
  type Columns (AHNonEmpty context a) = HNonEmptyTable (Columns a)
  type Unreify (AHNonEmpty context a) = HNonEmpty context (Unreify a)

  fromColumns = sfromColumnsNonEmpty contextSing
  toColumns = stoColumnsNonEmpty contextSing
  reify proof =
    smapNonEmpty contextSing (reify proof) hreify .
    AHNonEmpty
  unreify proof =
    (\(AHNonEmpty a) -> a) .
    smapNonEmpty contextSing (unreify proof) hunreify


instance
  ( Reifiable context, Reifiable context'
  , Unreifiable (Reify context) a, Unreifiable (Reify context') a'
  , Recontextualize (Reify context) (Reify context') a a'
  ) =>
  Recontextualize
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
  SInsert -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
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
  SInsert -> AHNonEmpty . NonEmptyTable
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
  SInsert -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SName -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SReify context ->
    hreify .
    stoColumnsNonEmpty context .
    smapNonEmpty context (hunreify . toColumns) hunreify .
    (\(AHNonEmpty a) -> a)
