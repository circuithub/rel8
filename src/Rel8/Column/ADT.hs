{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.ADT
  ( HADT, AHADT(..)
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Generic.Rel8able ( GColumns )
import Rel8.Kind.Context ( SContext(..), Reifiable, contextSing )
import Rel8.Schema.Context ( Col )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.ADT ( ADT( ADT ), ADTable, fromADT, toADT )
import Rel8.Table.Recontextualize ( Recontextualize )


type HADT :: K.Context -> K.Rel8able -> Type
type family HADT context t where
  HADT (Reify context) t = AHADT context t
  HADT Result t = t Result
  HADT context t = ADT t context


type AHADT :: K.Context -> K.Rel8able -> Type
newtype AHADT context t = AHADT (HADT context t)


instance (ADTable t, Reifiable context) =>
  Table (Reify context) (AHADT context t)
 where
  type Context (AHADT context t) = Reify context
  type Columns (AHADT context t) = GColumns (ADT t)
  type Unreify (AHADT context t) = HADT context t

  fromColumns = sfromColumnsADT contextSing
  toColumns = stoColumnsADT contextSing
  reify _ = AHADT
  unreify _ (AHADT a) = a


instance
  ( Reifiable context, Reifiable context'
  , ADTable t, t ~ t'
  )
  => Recontextualize
    (Reify context)
    (Reify context')
    (AHADT context t)
    (AHADT context' t')


sfromColumnsADT :: ADTable t
  => SContext context
  -> GColumns (ADT t) (Col (Reify context))
  -> AHADT context t
sfromColumnsADT = \case
  SAggregate -> AHADT . ADT . hunreify
  SExpr -> AHADT . ADT . hunreify
  SInsert -> AHADT . ADT . hunreify
  SName -> AHADT . ADT . hunreify
  SResult -> AHADT . fromADT . ADT . hunreify
  SReify context -> AHADT . sfromColumnsADT context . hunreify


stoColumnsADT :: ADTable t
  => SContext context
  -> AHADT context t
  -> GColumns (ADT t) (Col (Reify context))
stoColumnsADT = \case
  SAggregate -> hreify . (\(AHADT (ADT a)) -> a)
  SExpr -> hreify . (\(AHADT (ADT a)) -> a)
  SInsert -> hreify . (\(AHADT (ADT a)) -> a)
  SName -> hreify . (\(AHADT (ADT a)) -> a)
  SResult -> hreify . (\(ADT a) -> a) . toADT . (\(AHADT a) -> a)
  SReify context -> hreify . stoColumnsADT context . (\(AHADT a) -> a)
