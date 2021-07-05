{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.Maybe
  ( HMaybe, AHMaybe(..)
  )
where

-- base
import Control.Category ( id )
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ), apply )
import Prelude hiding ( id )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.Maybe ( HMaybeTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result, absurd )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify, coherence, congruence
  )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.Recontextualize ( Recontextualize )


-- | Nest a 'Maybe' value within a 'Rel8able'. @HMaybe f a@ will produce a
-- 'MaybeTable' @a@ in the 'Expr' context, and a 'Maybe' @a@ in the 'Result'
-- context.
type HMaybe :: K.Context -> Type -> Type
type family HMaybe context = maybe | maybe -> context where
  HMaybe (Reify context) = AHMaybe context
  HMaybe Aggregate = MaybeTable Aggregate
  HMaybe Expr = MaybeTable Expr
  HMaybe Name = MaybeTable Name
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
  SName ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SReify context ->
    hreify .
    stoColumnsMaybe context .
    smapMaybe context (hunreify . toColumns) .
    (\(AHMaybe a) -> a)
