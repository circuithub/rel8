{-# language ConstrainedClassMethods #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Table.Lifted
  ( Table1( Columns1, ConstrainContext1, toColumns1, fromColumns1 )
  , Table2( Columns2, ConstrainContext2, toColumns2, fromColumns2 )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Schema.Context ( Interpretation, Col )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K


type Table1 :: (Type -> Type) -> Constraint
class Table1 f where
  type Columns1 f :: K.HTable -> K.HTable
  type ConstrainContext1 f :: K.Context -> Constraint
  type ConstrainContext1 _ = Interpretation

  toColumns1 :: (ConstrainContext1 f context, HTable t)
    => (a -> t (Col context))
    -> f a
    -> Columns1 f t (Col context)

  fromColumns1 :: (ConstrainContext1 f context, HTable t)
    => (t (Col context) -> a)
    -> Columns1 f t (Col context)
    -> f a


type Table2 :: (Type -> Type -> Type) -> Constraint
class Table2 p where
  type Columns2 p :: K.HTable -> K.HTable -> K.HTable
  type ConstrainContext2 p :: K.Context -> Constraint
  type ConstrainContext2 _ = Interpretation

  toColumns2 :: (ConstrainContext2 p context, HTable t, HTable u)
    => (a -> t (Col context))
    -> (b -> u (Col context))
    -> p a b
    -> Columns2 p t u (Col context)

  fromColumns2 :: (ConstrainContext2 p context, HTable t, HTable u)
    => (t (Col context) -> a)
    -> (u (Col context) -> b)
    -> Columns2 p t u (Col context)
    -> p a b
