{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.List
  ( HList, AHList(..)
  )
where

-- base
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.List ( HListTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Congruent, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.List ( ListTable(..) )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Unreify ( Unreifiability(..), Unreifiable, unreifiability )


-- | Nest a list within a 'Rel8able'. @HList f a@ will produce a 'ListTable'
-- @a@ in the 'Expr' context, and a @[a]@ in the 'Result' context.
type HList :: K.Context -> Type -> Type
type family HList context where
  HList (Reify context) = AHList context
  HList Aggregate = ListTable Aggregate
  HList Expr = ListTable Expr
  HList Name = ListTable Name
  HList Result = []


type AHList :: K.Context -> Type -> Type
newtype AHList context a = AHList (HList context a)


instance (Reifiable context, Unreifiable a, Table (Reify context) a) =>
  Table (Reify context) (AHList context a)
 where
  type Context (AHList context a) = Reify context
  type Columns (AHList context a) = HListTable (Columns a)
  type Unreify (AHList context a) = HList context (Unreify a)

  fromColumns = sfromColumnsList contextSing
  toColumns = stoColumnsList contextSing

  reify _ = sreifyList (unreifiability contextSing)
  unreify _ = sunreifyList (unreifiability contextSing)


instance
  ( Reifiable context, Reifiable context'
  , Unreifiable a, Unreifiable a'
  , Recontextualize (Reify context) (Reify context') a a'
  )
  => Recontextualize
    (Reify context)
    (Reify context')
    (AHList context a)
    (AHList context' a')


smapList :: Congruent a b
  => SContext context
  -> (a -> b)
  -> (HListTable (Columns a) (Col (Context a)) -> HListTable (Columns b) (Col (Context b)))
  -> AHList context a
  -> AHList context b
smapList = \case
  SAggregate -> \f _ -> \case
    AHList (ListTable g a) -> AHList (ListTable (f . g) a)

  SExpr -> \f _ -> \case
    AHList (ListTable g a) -> AHList (ListTable (f . g) a)

  SResult -> \f _ (AHList as) -> AHList (fmap f as)

  SName -> \f _ -> \case
    AHList (ListTable g a) -> AHList (ListTable (f . g) a)

  SReify context -> \f g (AHList as) -> AHList (smapList context f g as)


sfromColumnsList :: Table (Reify context) a
  => SContext context
  -> HListTable (Columns a) (Col (Reify context))
  -> AHList context a
sfromColumnsList = \case
  SAggregate -> AHList . ListTable (fromColumns . hreify) . hunreify
  SExpr -> AHList . ListTable (fromColumns . hreify) . hunreify
  SResult -> AHList . fmap (fromColumns . hreify) . fromColumns . hunreify
  SName -> AHList . ListTable (fromColumns . hreify) . hunreify
  SReify context ->
    AHList .
    smapList context (fromColumns . hreify) hreify .
    sfromColumnsList context .
    hunreify


stoColumnsList :: Table (Reify context) a
  => SContext context
  -> AHList context a
  -> HListTable (Columns a) (Col (Reify context))
stoColumnsList = \case
  SAggregate ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHList a) -> a)

  SExpr ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHList a) -> a)

  SResult ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHList a) -> a)

  SName ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHList a) -> a)

  SReify context ->
    hreify .
    stoColumnsList context .
    smapList context (hunreify . toColumns) hunreify .
    (\(AHList a) -> a)


sreifyList :: Table (Reify context) a
  => Unreifiability context a
  -> HList context (Unreify a)
  -> AHList context a
sreifyList = \case
  UResult -> AHList . fmap (reify Refl)
  Unreifiability context ->
    smapList context (reify Refl) hreify .
    AHList


sunreifyList :: Table (Reify context) a
  => Unreifiability context a
  -> AHList context a
  -> HList context (Unreify a)
sunreifyList = \case
  UResult -> fmap (unreify Refl) . (\(AHList a) -> a)
  Unreifiability context ->
    (\(AHList a) -> a) .
    smapList context (unreify Refl) hunreify
