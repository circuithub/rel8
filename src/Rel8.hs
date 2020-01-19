{-# options -fno-warn-duplicate-exports #-}

-- | Welcome to Rel8!
module Rel8
  ( -- * Schema Definition
    -- ** Defining Tables
    HigherKindedTable
  , Column
  , TableSchema(..)
  , ColumnSchema

    -- ** Defining Database Types
  , DBType( lit )
  , DBEq( eqExprs )

    -- * Writing Queries
  , MonadQuery
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
  , Expr
  , Context
  , coerceExpr
  , unsafeCoerceExpr

    -- *** Literals
  ,  lit

    -- *** Equality
  , EqTable
  , (==.)
  , Table

    -- *** Booleans
  , (&&.)
  , (||.)
  , not_

    -- *** Functions
  , dbFunction
  , nullaryFunction
  , Function

    -- * Running Queries
    -- ** @SELECT@
  , select
  , FromRow(..)
  , Query

    -- ** @INSERT@
  , insert
  , Insert(..)
  , Returning(..)

    -- ** @DELETE@
  , delete
  , Delete(..)

    -- ** @UPDATE@
  , update
  , Update(..)

    -- * Next Steps
    -- $nextSteps
  )
  where

import Rel8.Aggregate
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.DBEq
import Rel8.EqTable
import Rel8.Expr
import Rel8.FromRow
import Rel8.MaybeTable
import Rel8.MonadQuery
import Rel8.Query
import Rel8.Table
import Rel8.TableSchema

{- $nextSteps

You've now seen the essential parts of Rel8. But the story doesn't finish here!
Rel8 includes a suite of utilities and patterns to help you write queries, here
are some jumping off points for more reading:

* "Rel8.Tabulate" - exposes the 'Rel8.Tabulate.Tabulated' and
  'Rel8.Tabulate.Tabulation' primitives, which make it easy to write queries
  that are highly composable.

-}
