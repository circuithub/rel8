{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
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
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Congruent, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.List ( ListTable( ListTable ) )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Unreify ( Unreifiable )


type HList :: K.Context -> Type -> Type
type family HList context where
  HList (Reify context) = AHList context
  HList Aggregate = ListTable
  HList Expr = ListTable
  HList Insert = ListTable
  HList Name = ListTable
  HList Result = []


type AHList :: K.Context -> Type -> Type
newtype AHList context a = AHList (HList context a)


instance
  ( Reifiable context
  , Table (Reify context) a
  , Unreifiable (Reify context) a
  )
  => Table (Reify context) (AHList context a)
 where
  type Context (AHList context a) = Reify context
  type Columns (AHList context a) = HListTable (Columns a)
  type Unreify (AHList context a) = HList context (Unreify a)

  fromColumns = sfromColumnsList contextSing
  toColumns = stoColumnsList contextSing
  reify proof =
    smapList contextSing (reify proof) hreify .
    AHList
  unreify proof =
    (\(AHList a) -> a) .
    smapList contextSing (unreify proof) hunreify


instance
  ( Reifiable context, Reifiable context'
  , Unreifiable (Reify context) a, Unreifiable (Reify context') a'
  , Recontextualize (Reify context) (Reify context') a a'
  ) =>
  Recontextualize
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
  SAggregate -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SExpr -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SResult -> \f _ (AHList as) -> AHList (fmap f as)
  SInsert -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SName -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SReify context -> \f g (AHList as) -> AHList (smapList context f g as)


sfromColumnsList :: Table (Reify context) a
  => SContext context
  -> HListTable (Columns a) (Col (Reify context))
  -> AHList context a
sfromColumnsList = \case
  SAggregate -> AHList . ListTable
  SExpr -> AHList . ListTable
  SResult -> AHList . fmap (fromColumns . hreify) . fromColumns . hunreify
  SInsert -> AHList . ListTable
  SName -> AHList . ListTable
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
  SAggregate -> \(AHList (ListTable a)) -> a
  SExpr -> \(AHList (ListTable a)) -> a
  SResult ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHList a) -> a)
  SInsert -> \(AHList (ListTable a)) -> a
  SName -> \(AHList (ListTable a)) -> a
  SReify context ->
    hreify .
    stoColumnsList context .
    smapList context (hunreify . toColumns) hunreify .
    (\(AHList a) -> a)
