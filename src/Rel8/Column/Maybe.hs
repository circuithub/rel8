{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.Maybe
  ( HMaybe, AHMaybe(..)
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.Maybe ( HMaybeTable )
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.Recontextualize ( Recontextualize )


type HMaybe :: K.Context -> Type -> Type
type family HMaybe context where
  HMaybe (Reify context) = AHMaybe context
  HMaybe Aggregate = MaybeTable
  HMaybe Expr = MaybeTable
  HMaybe Insert = MaybeTable
  HMaybe Name = MaybeTable
  HMaybe Result = Maybe


type AHMaybe :: K.Context -> Type -> Type
newtype AHMaybe context a = AHMaybe (HMaybe context a)


instance Reifiable context => Functor (AHMaybe context) where
  fmap = smapMaybe contextSing


instance (Reifiable context, Table (Reify context) a) =>
  Table (Reify context) (AHMaybe context a)
 where
  type Context (AHMaybe context a) = Reify context
  type Columns (AHMaybe context a) = HMaybeTable (Columns a)
  type Unreify (AHMaybe context a) = HMaybe context (Unreify a)

  fromColumns = sfromColumnsMaybe contextSing
  toColumns = stoColumnsMaybe contextSing
  reify proof = fmap fmap reify proof . AHMaybe
  unreify proof = (\(AHMaybe a) -> a) . fmap fmap unreify proof


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHMaybe context a)
    (AHMaybe context' a')


smapMaybe :: ()
  => SContext context
  -> (a -> b)
  -> AHMaybe context a
  -> AHMaybe context b
smapMaybe = \case
  SAggregate -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SExpr -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SResult -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SInsert -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SName -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SReify context -> \f (AHMaybe a) -> AHMaybe (smapMaybe context f a)


sfromColumnsMaybe :: Table (Reify context) a
  => SContext context
  -> HMaybeTable (Columns a) (Col (Reify context))
  -> AHMaybe context a
sfromColumnsMaybe = \case
  SAggregate -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SExpr -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SResult -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SInsert -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SName -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SReify context ->
    AHMaybe .
    smapMaybe context (fromColumns . hreify) .
    sfromColumnsMaybe context .
    hunreify


stoColumnsMaybe :: Table (Reify context) a
  => SContext context
  -> AHMaybe context a
  -> HMaybeTable (Columns a) (Col (Reify context))
stoColumnsMaybe = \case
  SAggregate ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SExpr ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SResult ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SInsert ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SName ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SReify context ->
    hreify .
    stoColumnsMaybe context .
    smapMaybe context (hunreify . toColumns) .
    (\(AHMaybe a) -> a)
