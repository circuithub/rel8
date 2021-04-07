{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Lifted
  ( Table1( Columns1, ConstrainHContext1, toColumns1, fromColumns1 )
  , Table2( Columns2, ConstrainHContext2, toColumns2, fromColumns2 )
  , ConstrainContext
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Pair
import qualified Rel8.Schema.Kind as K


type Table1 :: (Type -> Type) -> Constraint
class Table1 f where
  type Columns1 f :: K.HTable -> K.HTable
  type ConstrainHContext1 f :: K.HContext -> Constraint
  type ConstrainHContext1 _ = DefaultConstrainContext

  toColumns1 :: (ConstrainHContext1 f context, HTable t)
    => (a -> t context)
    -> f a
    -> Columns1 f t context

  fromColumns1 :: (ConstrainHContext1 f context, HTable t)
    => (t context -> a)
    -> Columns1 f t context
    -> f a


type Table2 :: (Type -> Type -> Type) -> Constraint
class Table2 p where
  type Columns2 p :: K.HTable -> K.HTable -> K.HTable
  type ConstrainHContext2 p :: K.HContext -> Constraint
  type ConstrainHContext2 _ = DefaultConstrainContext

  toColumns2 :: (ConstrainHContext2 p context, HTable t, HTable u)
    => (a -> t context)
    -> (b -> u context)
    -> p a b
    -> Columns2 p t u context

  fromColumns2 :: (ConstrainHContext2 p context, HTable t, HTable u)
    => (t context -> a)
    -> (u context -> b)
    -> Columns2 p t u context
    -> p a b


instance Table2 (,) where
  type Columns2 (,) = HPair

  toColumns2 f g (a, b) = HPair (f a) (g b)
  fromColumns2 f g (HPair a b) = (f a, g b)


type DefaultConstrainContext :: K.HContext -> Constraint
class DefaultConstrainContext context
instance DefaultConstrainContext context


type ConstrainContext :: (K.Context -> Constraint) -> K.HContext -> Constraint
class (forall context. hcontext ~ Col context => constraint context)
  => ConstrainContext constraint hcontext
instance (hcontext ~ Col context, constraint context) =>
  ConstrainContext constraint hcontext
