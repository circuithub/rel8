{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.These
  ( HThese, AHThese(..)
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
import Rel8.Schema.HTable.These ( HTheseTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.These ( TheseTable )

-- these
import Data.These ( These )


-- | Nest an 'These' value within a 'Rel8able'. @HThese f a b@ will produce a
-- 'TheseTable' @a b@ in the 'Expr' context, and a 'These' @a b@ in the
-- 'Result' context.
type HThese :: K.Context -> Type -> Type -> Type
type family HThese context = these | these -> context where
  HThese (Reify context) = AHThese context
  HThese Aggregate = TheseTable Aggregate
  HThese Expr = TheseTable Expr
  HThese Name = TheseTable Name
  HThese Result = These


type AHThese :: K.Context -> Type -> Type -> Type
newtype AHThese context a b = AHThese (HThese context a b)


instance Reifiable context => Bifunctor (AHThese context) where
  bimap = sbimapThese contextSing


instance Reifiable context => Functor (AHThese context a) where
  fmap = bimap id


instance (Reifiable context, Table (Reify context) a, Table (Reify context) b)
  => Table (Reify context) (AHThese context a b)
 where
  type Context (AHThese context a b) = Reify context
  type Columns (AHThese context a b) = HTheseTable (Columns a) (Columns b)
  type Unreify (AHThese context a b) = HThese context (Unreify a) (Unreify b)

  fromColumns = sfromColumnsThese contextSing
  toColumns = stoColumnsThese contextSing
  reify proof = liftA2 bimap reify reify proof . AHThese
  unreify proof = (\(AHThese a) -> a) . liftA2 bimap unreify unreify proof


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  , Recontextualize (Reify context) (Reify context') b b'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHThese context a b)
    (AHThese context' a' b')


sbimapThese :: ()
  => SContext context
  -> (a -> c)
  -> (b -> d)
  -> AHThese context a b
  -> AHThese context c d
sbimapThese = \case
  SAggregate -> \f g (AHThese a) -> AHThese (bimap f g a)
  SExpr -> \f g (AHThese a) -> AHThese (bimap f g a)
  SResult -> \f g (AHThese a) -> AHThese (bimap f g a)
  SName -> \f g (AHThese a) -> AHThese (bimap f g a)
  SReify context -> \f g (AHThese a) -> AHThese (sbimapThese context f g a)


sfromColumnsThese :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> HTheseTable (Columns a) (Columns b) (Col (Reify context))
  -> AHThese context a b
sfromColumnsThese = \case
  SAggregate ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SExpr ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SResult ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SName ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SReify context ->
    AHThese .
    sbimapThese context (fromColumns . hreify) (fromColumns . hreify) .
    sfromColumnsThese context .
    hunreify


stoColumnsThese :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> AHThese context a b
  -> HTheseTable (Columns a) (Columns b) (Col (Reify context))
stoColumnsThese = \case
  SAggregate ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SExpr ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SResult ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SName ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SReify context ->
    hreify .
    stoColumnsThese context .
    sbimapThese context (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
