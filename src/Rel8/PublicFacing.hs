module Rel8.PublicFacing
  ( -- * Schema Definition
    -- ** Defining Tables
    HigherKinded
  , mapRecord
  , Column
  , TableSchema(..)
  , ColumnSchema

    -- ** Defining Database Types
  , DBTypeEq(..)

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
  , maybeTable

    -- ** Equality
  , EqTable
  , (==.)
  , Table

    -- * Running Queries
  , Query
  , select
  )
  where

import Rel8.Column
import Rel8.ColumnSchema
import Rel8.TableSchema
import Rel8.HigherKinded
import Rel8.EqTable
import Rel8.Table
import Rel8.MonadQuery
import Rel8.DBEq
import Rel8.Expr
import Rel8.MaybeTable
import Rel8.Query