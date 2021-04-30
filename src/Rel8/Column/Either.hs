{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.Either
  ( HEither, AHEither(..)
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..), Reifiable( contextSing ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable.Either ( HEitherTable )
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..) )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.Recontextualize ( Recontextualize )


type HEither :: K.Context -> Type -> Type -> Type
type family HEither context where
  HEither (Reify context) = AHEither context
  HEither Aggregate = EitherTable
  HEither Expr = EitherTable
  HEither Insert = EitherTable
  HEither Name = EitherTable
  HEither Result = Either


type AHEither :: K.Context -> Type -> Type -> Type
newtype AHEither context a b = AHEither (HEither context a b)


instance Reifiable context => Bifunctor (AHEither context) where
  bimap = sbimapEither contextSing


instance Reifiable context => Functor (AHEither context a) where
  fmap = bimap id


instance (Reifiable context, Table (Reify context) a, Table (Reify context) b)
  => Table (Reify context) (AHEither context a b)
 where
  type Context (AHEither context a b) = Reify context
  type Columns (AHEither context a b) = HEitherTable (Columns a) (Columns b)
  type Unreify (AHEither context a b) = HEither context (Unreify a) (Unreify b)

  fromColumns = sfromColumnsEither contextSing
  toColumns = stoColumnsEither contextSing
  reify proof = liftA2 bimap reify reify proof . AHEither
  unreify proof = (\(AHEither a) -> a) . liftA2 bimap unreify unreify proof


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  , Recontextualize (Reify context) (Reify context') b b'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHEither context a b)
    (AHEither context' a' b')


sbimapEither :: ()
  => SContext context
  -> (a -> c)
  -> (b -> d)
  -> AHEither context a b
  -> AHEither context c d
sbimapEither = \case
  SAggregate -> \f g (AHEither a) -> AHEither (bimap f g a)
  SExpr -> \f g (AHEither a) -> AHEither (bimap f g a)
  SResult -> \f g (AHEither a) -> AHEither (bimap f g a)
  SInsert -> \f g (AHEither a) -> AHEither (bimap f g a)
  SName -> \f g (AHEither a) -> AHEither (bimap f g a)
  SReify context -> \f g (AHEither a) -> AHEither (sbimapEither context f g a)


sfromColumnsEither :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> HEitherTable (Columns a) (Columns b) (Col (Reify context))
  -> AHEither context a b
sfromColumnsEither = \case
  SAggregate ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SExpr ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SResult ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SInsert ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SName ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SReify context ->
    AHEither .
    sbimapEither context (fromColumns . hreify) (fromColumns . hreify) .
    sfromColumnsEither context .
    hunreify


stoColumnsEither :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> AHEither context a b
  -> HEitherTable (Columns a) (Columns b) (Col (Reify context))
stoColumnsEither = \case
  SAggregate ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SExpr ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SResult ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SInsert ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SName ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SReify context ->
    hreify .
    stoColumnsEither context .
    sbimapEither context (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
