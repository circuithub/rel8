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
  , DBType( typeInformation )
  , DatabaseType(..)
  , parseDatabaseType
  , DBEq( eqExprs )

    -- * Writing Queries
  , Table
  , each
  , values
  , where_
  , limit
  , offset
  , union
  , distinct
  , exists

    -- ** Optional Subqueries
  , optional
  , MaybeTable
  , maybeTable
  , catMaybeTable
  , noTable

    -- ** Expressions
  , Expr
  , Context
  , coerceExpr
  , unsafeCoerceExpr

    -- *** Literals
  , lit

    -- *** Null
  , null_
  , isNull
  , liftNull

    -- *** Equality
  , EqTable( (==.) )

    -- *** Booleans
  , (&&.)
  , (||.)
  , not_
  , ifThenElse_

    -- *** Functions
  , dbFunction
  , nullaryFunction
  , Function

    -- * Running Queries
    -- ** @SELECT@
  , select
  , FromRow(..)
  , Query
  , Identity

    -- ** @INSERT@
  , insert
  , Insert(..)
  , Returning(..)
  , OnConflict(..)
  , litTable

    -- ** @DELETE@
  , delete
  , Delete(..)

    -- ** @UPDATE@
  , update
  , Update(..)

    -- * Common Table Subtypes
  , IsTableIn
  , Selects

    -- * Next Steps
    -- $nextSteps
  )
  where

import Data.Functor.Identity
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.DBEq
import Rel8.DBType
import Rel8.EqTable
import Rel8.Expr
import Rel8.FromRow
import Rel8.Lit
import Rel8.MaybeTable
import Rel8.Query
import Rel8.Table
import Rel8.TableSchema
import Rel8.SimpleConstraints

{- $nextSteps

You've now seen the essential parts of Rel8. But the story doesn't finish here!
Rel8 includes a suite of utilities and patterns to help you write queries, here
are some jumping off points for more reading:

* "Rel8.Tabulate" - exposes the 'Rel8.Tabulate.Tabulated' and
  'Rel8.Tabulate.Tabulation' primitives, which make it easy to write queries
  that are highly composable.

-}
