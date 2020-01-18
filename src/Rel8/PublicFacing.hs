{-# options -fno-warn-duplicate-exports #-}

-- | Welcome to Rel8!
module Rel8.PublicFacing
  ( -- * Schema Definition
    -- ** Defining Tables
    HigherKinded
  , Column
  , TableSchema(..)
  , ColumnSchema

    -- ** Defining Database Types
  , DBType( lit )
  , DBEq( eqExprs )

    -- * Writing Queries
  , MonadQuery
  , Expr
  , each
  , where_
  , limit
  , offset
  , union
  , distinct
  , exists

    -- ** Outer Joins
  , leftJoin
  , MaybeTable
  , isNull
  , toMaybe

    -- ** Aggregation
  , groupAndAggregate
  , aggregate
  , GroupBy(..)
  , MonoidTable
  , DBMonoid

    -- ** Expressions
    -- *** Literals
  ,  lit

    -- *** Equality
  , EqTable
  , (==.)
  , Table

    -- *** Booleans
  , (&&.)
  , (||.)

    -- * Running Queries
  , select
  , FromRow(..)
  , Query

  )
  where

import Rel8.Aggregate
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.DBEq
import Rel8.EqTable
import Rel8.Expr
import Rel8.FromRow
import Rel8.HigherKinded
import Rel8.MaybeTable
import Rel8.MonadQuery
import Rel8.Query
import Rel8.Table
import Rel8.TableSchema
