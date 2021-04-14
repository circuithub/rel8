{-# language DuplicateRecordFields #-}

module Rel8
  ( -- * Database types
    -- ** @DBType@
    DBType(..)

    -- *** Deriving-via helpers
    -- **** @JSONEncoded@
  , JSONEncoded(..)
  , JSONBEncoded(..)

    -- **** @ReadShow@
  , ReadShow(..)

    -- *** @TypeInformation@
  , TypeInformation(..)
  , mapTypeInformation
  , parseTypeInformation

    -- ** The @DBType@ hierarchy
  , DBSemigroup(..)
  , DBMonoid(..)
  , DBNum
  , DBIntegral
  , DBFractional

    -- * Tables and higher-kinded tables
  , Rel8able
  , Column, Field, Necessity( Required, Optional )
  , Default
  , HMaybe
  , HList
  , HNonEmpty
  , HThese

  , Table(..)
  , AltTable((<|>:))
  , EqTable, (==:), (/=:)
  , OrdTable, ascTable, descTable
  , lit
  , bool
  , case_

    -- ** @MaybeTable@
  , MaybeTable
  , maybeTable, ($?), nothingTable, justTable
  , isNothingTable, isJustTable
  , optional
  , catMaybeTable
  , bindMaybeTable
  , traverseMaybeTable

    -- ** @EitherTable@
  , EitherTable
  , eitherTable, leftTable, rightTable
  , isLeftTable, isRightTable
  , keepLeftTable
  , keepRightTable
  , bindEitherTable
  , bitraverseEitherTable

    -- ** @TheseTable@
  , TheseTable
  , theseTable, thisTable, thatTable, thoseTable
  , isThisTable, isThatTable, isThoseTable
  , hasHereTable, hasThereTable
  , justHereTable, justThereTable
  , alignBy
  , keepHereTable, loseHereTable
  , keepThereTable, loseThereTable
  , keepThisTable, loseThisTable
  , keepThatTable, loseThatTable
  , keepThoseTable, loseThoseTable
  , bindTheseTable
  , bitraverseTheseTable

    -- ** @ListTable@
  , ListTable
  , many

    -- ** @NonEmptyTable@
  , NonEmptyTable
  , some

    -- ** Table schemas
  , TableSchema(..)
  , Name
  , namesFromLabels
  , namesFromLabelsWith

    -- * Expressions
  , Expr
  , Sql
  , litExpr
  , unsafeCastExpr

    -- ** @null@
  , null
  , nullify
  , nullable
  , isNull
  , isNonNull
  , mapNull
  , liftOpNull
  , catNull
  , coalesce

    -- ** Boolean operations
  , DBEq
  , true, false, not_
  , (&&.), and_
  , (||.), or_
  , (==.), (/=.), (==?), (/=?)
  , in_
  , boolExpr, caseExpr

    -- ** Ordering
  , DBOrd
  , (<.), (<=.), (>.), (>=.)
  , (<?), (<=?), (>?), (>=?)
  , leastExpr, greatestExpr

    -- ** Functions
  , Function
  , function
  , nullaryFunction
  , binaryOperator

    -- * Queries
  , Query
  , showQuery

    -- ** Selecting rows
  , Selects
  , each
  , values

    -- ** Filtering
  , filter
  , where_
  , whereExists
  , whereNotExists
  , distinct
  , distinctOn
  , distinctOnBy

    -- ** @LIMIT@/@OFFSET@
  , limit
  , offset

    -- ** @UNION@
  , union
  , unionAll

    -- ** @INTERSECT@
  , intersect
  , intersectAll

    -- ** @EXCEPT@
  , except
  , exceptAll

    -- ** @EXISTS@
  , exists
  , with
  , withBy
  , without
  , withoutBy

    -- ** Aggregation
  , Aggregate
  , aggregate
  , countRows
  , array1DAggExpr
  , listAgg, listAggExpr
  , nonEmptyAgg, nonEmptyAggExpr
  , groupBy
  , DBMax, max
  , DBMin, min
  , DBSum, sum, sumWhere
  , DBString, stringAgg
  , count
  , countStar
  , countDistinct
  , countWhere
  , and
  , or

    -- ** Ordering
  , orderBy
  , Order
  , asc
  , desc
  , nullsFirst
  , nullsLast

    -- * IO
  , Serializable

    -- * Running statements
    -- ** @SELECT@
  , select

    -- ** @INSERT@
  , Insert(..)
  , OnConflict(..)
  , insert
  , toInsert
  , toInsertDefaults

    -- ** @DELETE@
  , Delete(..)
  , delete

    -- ** @UPDATE@
  , update
  , Update(..)

    -- ** @.. RETURNING@
  , Returning(..)

    -- ** @CREATE VIEW@
  , createView

    -- * TODO
    -- TODO
    -- These need organizing, but are reachable from Rel8's documentation so we
    -- do need to export and document them.
  , Array1D
  , Nullable
  , NotArray
  , NotNull
  , HTable
  , Labelable
  , ToExprs(..)
  , FromExprs
  ) where

-- base
import Prelude ()

-- rel8
import Rel8.Aggregate
import Rel8.Expr
import Rel8.Expr.Aggregate
import Rel8.Expr.Bool
import Rel8.Expr.Eq
import Rel8.Expr.Function
import Rel8.Expr.Null
import Rel8.Expr.Opaleye (unsafeCastExpr)
import Rel8.Expr.Ord
import Rel8.Expr.Order
import Rel8.Expr.Serialize
import Rel8.Kind.Necessity
import Rel8.Order
import Rel8.Query
import Rel8.Query.Aggregate
import Rel8.Query.Distinct
import Rel8.Query.Each
import Rel8.Query.Either
import Rel8.Query.Exists
import Rel8.Query.Filter
import Rel8.Query.Limit
import Rel8.Query.List
import Rel8.Query.Maybe
import Rel8.Query.Null
import Rel8.Query.Order
import Rel8.Query.SQL (showQuery)
import Rel8.Query.Set
import Rel8.Query.These
import Rel8.Query.Values
import Rel8.Schema.Column
import Rel8.Schema.Context.Label
import Rel8.Schema.Field
import Rel8.Schema.Generic
import Rel8.Schema.HTable
import Rel8.Schema.Name
import Rel8.Schema.Null hiding ( nullable )
import Rel8.Schema.Table
import Rel8.Statement.Delete
import Rel8.Statement.Insert
import Rel8.Statement.Returning
import Rel8.Statement.Select
import Rel8.Statement.Update
import Rel8.Statement.View
import Rel8.Table
import Rel8.Table.Aggregate
import Rel8.Table.Alternative
import Rel8.Table.Bool
import Rel8.Table.Either
import Rel8.Table.Eq
import Rel8.Table.Insert
import Rel8.Table.List
import Rel8.Table.Maybe
import Rel8.Table.Name
import Rel8.Table.NonEmpty
import Rel8.Table.Ord
import Rel8.Table.Order
import Rel8.Table.Serialize
import Rel8.Table.These
import Rel8.Type
import Rel8.Type.Array1D
import Rel8.Type.Eq
import Rel8.Type.Information
import Rel8.Type.JSONBEncoded
import Rel8.Type.JSONEncoded
import Rel8.Type.Monoid
import Rel8.Type.Num
import Rel8.Type.Ord
import Rel8.Type.ReadShow
import Rel8.Type.Semigroup
import Rel8.Type.String
import Rel8.Type.Sum
