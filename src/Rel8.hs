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

    -- **** Generic
  , Composite(..), DBComposite(..), compose, decompose
  , Enum(..), DBEnum(..), Enumable

    -- *** @TypeInformation@
  , TypeInformation(..)
  , TypeName(..)
  , mapTypeInformation
  , parseTypeInformation

    -- *** @Decoder@
  , Decoder(..)

    -- ** The @DBType@ hierarchy
  , DBSemigroup(..)
  , DBMonoid(..)
  , DBNum
  , DBIntegral
  , DBFractional
  , DBFloating

    -- * Tables and higher-kinded tables
  , Rel8able, KRel8able
  , Column
  , HADT
  , HEither
  , HMaybe
  , HList
  , HNonEmpty
  , HNull
  , HThese
  , Lift

  , Table(..)
  , HTable
  , Transposes
  , AltTable((<|>:))
  , AlternativeTable( emptyTable )
  , EqTable(..), (==:), (/=:)
  , OrdTable(..), (<:), (<=:), (>:), (>=:), ascTable, descTable, greatest, least
  , lit
  , bool
  , case_
  , castTable

    -- ** @MaybeTable@
  , MaybeTable
  , maybeTable, ($?), nothingTable, justTable
  , isNothingTable, isJustTable
  , fromMaybeTable
  , optional
  , catMaybeTable
  , traverseMaybeTable
  , aggregateJustTable, aggregateJustTable1
  , aggregateMaybeTable
  , nameMaybeTable

    -- ** @EitherTable@
  , EitherTable
  , eitherTable, leftTable, rightTable
  , isLeftTable, isRightTable
  , keepLeftTable
  , keepRightTable
  , bitraverseEitherTable
  , aggregateLeftTable, aggregateLeftTable1
  , aggregateRightTable, aggregateRightTable1
  , aggregateEitherTable
  , nameEitherTable

    -- ** @TheseTable@
  , TheseTable
  , theseTable, thisTable, thatTable, thoseTable
  , isThisTable, isThatTable, isThoseTable
  , hasHereTable, hasThereTable
  , justHereTable, justThereTable
  , alignMaybeTable
  , alignBy
  , keepHereTable, loseHereTable
  , keepThereTable, loseThereTable
  , keepThisTable, loseThisTable
  , keepThatTable, loseThatTable
  , keepThoseTable, loseThoseTable
  , bitraverseTheseTable
  , aggregateThisTable, aggregateThisTable1
  , aggregateThatTable, aggregateThatTable1
  , aggregateThoseTable, aggregateThoseTable1
  , aggregateHereTable, aggregateHereTable1
  , aggregateThereTable, aggregateThereTable1
  , aggregateTheseTable
  , nameTheseTable

    -- ** @ListTable@
  , ListTable
  , listOf, listTable, ($*)
  , nameListTable
  , many
  , manyExpr
  , catListTable
  , catList

    -- ** @NonEmptyTable@
  , NonEmptyTable
  , nonEmptyOf, nonEmptyTable, ($+)
  , nameNonEmptyTable
  , some
  , someExpr
  , catNonEmptyTable
  , catNonEmpty

    -- ** @NullTable@
  , NullTable
  , nullableTable, nullTable, nullifyTable
  , isNullTable, isNonNullTable
  , catNullTable
  , nameNullTable
  , toNullTable, toMaybeTable
  , unsafeUnnullifyTable

    -- ** Algebraic data types / sum types
    -- $adts

    -- *** Naming of ADTs
    -- $naming
  , NameADT, nameADT
  , ADT, ADTable

    -- *** Deconstruction of ADTs
    -- $deconstruction
  , DeconstructADT, deconstructADT

    -- *** Construction of ADTs
    -- $construction
  , BuildADT, buildADT
  , ConstructADT, constructADT

    -- *** Miscellaneous notes
    -- $misc-notes

    -- ** @HKD@
  , HKD, HKDable
  , BuildHKD, buildHKD
  , ConstructHKD, constructHKD
  , DeconstructHKD, deconstructHKD
  , NameHKD, nameHKD

    -- ** Table schemas
  , TableSchema(..)
  , QualifiedName(..)
  , Name
  , namesFromLabels
  , namesFromLabelsWith

    -- * Expressions
  , Expr
  , Sql
  , litExpr
  , unsafeCastExpr
  , unsafeCoerceExpr
  , unsafeLiteral
  , unsafePrimExpr

    -- ** @null@
  , NotNull
  , Nullable
  , Homonullable
  , null
  , nullify
  , nullable
  , isNull
  , isNonNull
  , mapNull
  , liftOpNull
  , catNull
  , coalesce
  , unsafeUnnullify

    -- ** Boolean operations
  , DBEq
  , true, false, not_
  , (&&.), and_
  , (||.), or_
  , (==.), (/=.), (==?), (/=?)
  , in_
  , boolExpr, caseExpr
  , like, ilike

    -- ** Ordering
  , DBOrd
  , (<.), (<=.), (>.), (>=.)
  , (<?), (<=?), (>?), (>=?)
  , leastExpr, greatestExpr

    -- ** Functions
  , Arguments
  , function
  , binaryOperator
  , queryFunction
  , rawFunction
  , rawBinaryOperator

    -- * Queries
  , Query
  , showQuery

    -- ** Projection
  , Projection
  , Projectable( project )
  , Biprojectable( biproject )
  , Projecting
  , Field

    -- ** Selecting rows
  , Selects
  , each
  , values

    -- ** Filtering
  , filter
  , where_
  , present
  , absent
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

    -- ** @WITH@
  , materialize

    -- ** @WITH RECURSIVE@
  , loop
  , loopDistinct

    -- ** Aggregation
  , Aggregator
  , Aggregator1
  , Aggregator'
  , Fold (Semi, Full)
  , toAggregator
  , toAggregator1
  , aggregate
  , aggregate1
  , filterWhere
  , filterWhereOptional
  , distinctAggregate
  , orderAggregateBy
  , optionalAggregate
  , countRows
  , groupBy, groupByOn
  , listAgg, listAggOn, listAggExpr, listAggExprOn
  , listCat, listCatOn, listCatExpr, listCatExprOn
  , nonEmptyAgg, nonEmptyAggOn, nonEmptyAggExpr, nonEmptyAggExprOn
  , nonEmptyCat, nonEmptyCatOn, nonEmptyCatExpr, nonEmptyCatExprOn
  , DBMax, max, maxOn
  , DBMin, min, minOn
  , DBSum, sum, sumOn, sumWhere, avg, avgOn
  , DBString, stringAgg
  , count, countOn
  , countStar
  , countDistinct, countDistinctOn
  , countWhere, countWhereOn
  , and, andOn
  , or, orOn
  , aggregateFunction
  , rawAggregateFunction

  , mode, modeOn
  , percentile, percentileOn
  , percentileContinuous, percentileContinuousOn
  , hypotheticalRank
  , hypotheticalDenseRank
  , hypotheticalPercentRank
  , hypotheticalCumeDist

    -- ** Ordering
  , orderBy
  , Order
  , asc
  , desc
  , nullsFirst
  , nullsLast

    -- ** Window functions
  , Window
  , window
  , Partition
  , over
  , partitionBy
  , orderPartitionBy
  , cumulative
  , currentRow
  , rowNumber
  , rank
  , denseRank
  , percentRank
  , cumeDist
  , ntile
  , lag, lagOn
  , lead, leadOn
  , firstValue, firstValueOn
  , lastValue, lastValueOn
  , nthValue, nthValueOn
  , indexed

    -- ** Bindings
  , rebind

    -- * IO
  , Serializable
  , ToExprs
  , Result

    -- * Running statements
    -- $running
  , run
  , run_
  , runN
  , run1
  , runMaybe
  , runVector

    -- ** @SELECT@
  , select

    -- ** @INSERT@
  , Insert(..)
  , OnConflict(..)
  , Upsert(..)
  , insert
  , unsafeDefault
  , showInsert

    -- ** @DELETE@
  , Delete(..)
  , delete
  , showDelete

    -- ** @UPDATE@
  , Update(..)
  , update
  , showUpdate

    -- ** @.. RETURNING@
  , Returning(..)

    -- ** @WITH@
  , Statement
  , showStatement

    -- ** @CREATE VIEW@
  , createView
  , createOrReplaceView

    -- ** Sequences
  , nextval
  , evaluate
  ) where

-- base
import Prelude ()

-- rel8
import Rel8.Aggregate
import Rel8.Aggregate.Fold
import Rel8.Aggregate.Function
import Rel8.Column
import Rel8.Column.ADT
import Rel8.Column.Either
import Rel8.Column.Lift
import Rel8.Column.List
import Rel8.Column.Maybe
import Rel8.Column.NonEmpty
import Rel8.Column.Null
import Rel8.Column.These
import Rel8.Expr
import Rel8.Expr.Aggregate
import Rel8.Expr.Array
import Rel8.Expr.Bool
import Rel8.Expr.Default
import Rel8.Expr.Eq
import Rel8.Expr.Function
import Rel8.Expr.Null
import Rel8.Expr.Opaleye (unsafeCastExpr, unsafeCoerceExpr, unsafeLiteral, unsafePrimExpr)
import Rel8.Expr.Ord
import Rel8.Expr.Order
import Rel8.Expr.Serialize
import Rel8.Expr.Sequence
import Rel8.Expr.Text ( like, ilike )
import Rel8.Expr.Window
import Rel8.Generic.Rel8able ( KRel8able, Rel8able )
import Rel8.Order
import Rel8.Query
import Rel8.Query.Aggregate
import Rel8.Query.Distinct
import Rel8.Query.Each
import Rel8.Query.Either
import Rel8.Query.Evaluate
import Rel8.Query.Exists
import Rel8.Query.Filter
import Rel8.Query.Function
import Rel8.Query.Indexed
import Rel8.Query.Limit
import Rel8.Query.List
import Rel8.Query.Loop
import Rel8.Query.Materialize
import Rel8.Query.Maybe
import Rel8.Query.Null
import Rel8.Query.Order
import Rel8.Query.Rebind
import Rel8.Query.SQL (showQuery)
import Rel8.Query.Set
import Rel8.Query.These
import Rel8.Query.Values
import Rel8.Query.Window
import Rel8.Schema.Field
import Rel8.Schema.HTable
import Rel8.Schema.Name
import Rel8.Schema.Null hiding ( nullable )
import Rel8.Schema.QualifiedName
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Table
import Rel8.Statement
import Rel8.Statement.Delete
import Rel8.Statement.Insert
import Rel8.Statement.OnConflict
import Rel8.Statement.Returning
import Rel8.Statement.Run
import Rel8.Statement.Select
import Rel8.Statement.SQL
import Rel8.Statement.Update
import Rel8.Statement.View
import Rel8.Table
import Rel8.Table.ADT
import Rel8.Table.Aggregate
import Rel8.Table.Aggregate.Maybe
import Rel8.Table.Alternative
import Rel8.Table.Bool
import Rel8.Table.Either
import Rel8.Table.Eq
import Rel8.Table.HKD
import Rel8.Table.List
import Rel8.Table.Maybe
import Rel8.Table.Name
import Rel8.Table.NonEmpty
import Rel8.Table.Null
import Rel8.Table.Opaleye ( castTable )
import Rel8.Table.Ord
import Rel8.Table.Order
import Rel8.Table.Projection
import Rel8.Table.Rel8able ()
import Rel8.Table.Serialize
import Rel8.Table.These
import Rel8.Table.Transpose
import Rel8.Table.Window
import Rel8.Type
import Rel8.Type.Composite
import Rel8.Type.Decoder
import Rel8.Type.Eq
import Rel8.Type.Enum
import Rel8.Type.Information
import Rel8.Type.JSONBEncoded
import Rel8.Type.JSONEncoded
import Rel8.Type.Monoid
import Rel8.Type.Name
import Rel8.Type.Num
import Rel8.Type.Ord
import Rel8.Type.ReadShow
import Rel8.Type.Semigroup
import Rel8.Type.String
import Rel8.Type.Sum
import Rel8.Window


-- $running
-- To run queries and otherwise interact with a PostgreSQL database, Rel8
-- provides the @run@ functions. These produce a 'Hasql.Statement.Statement's
-- which can be passed to 'Hasql.Session.statement' to execute the statement
-- against a PostgreSQL 'Hasql.Connection.Connection'.
--
-- 'run' takes a 'Statement', which can be constructed using either 'select',
-- 'insert', 'update' or 'delete'. It decodes the rows returned by the
-- statement as a list of Haskell of values. See 'run_', 'runN', 'run1',
-- 'runMaybe' and 'runVector' for other variations.
--
-- Note that constructing an 'Insert', 'Update' or 'Delete' will require the
-- @DisambiguateRecordFields@ language extension to be enabled.

-- $adts
-- Algebraic data types can be modelled between Haskell and SQL.
--
-- * Your SQL table needs a certain text field that tags which Haskell constructor is in use.
-- * You have to use a few combinators to specify the sum type's individual constructors.
-- * If you want to do case analysis at the @Expr@ (SQL) level, you can use 'maybe'/'either'-like eliminators.
--
-- The documentation in this section will assume a set of database types like this:
--
-- @
-- data Thing f = ThingEmployer (Employer f) | ThingPotato (Potato f) | Nullary
--     deriving stock Generic
--
-- data Employer f = Employer { employerId :: f Int32, employerName :: f Text}
--   deriving stock Generic
--   deriving anyclass Rel8able
--
-- data Potato f = Potato { size :: f Int32, grower :: f Text }
--   deriving stock Generic
--   deriving anyclass Rel8able
-- @

-- $naming
--
-- First, in your 'TableSchema', name your type like this:
--
-- @
-- thingSchema :: TableSchema (ADT Thing Name)
-- thingSchema =
--   TableSchema
--     { name = \"thing\",
--       columns =
--         nameADT @Thing
--           \"tag\"
--           Employer
--             { employerName = \"name\",
--               employerId = \"id\"
--             }
--           Potato {size = \"size\", grower = \"Mary\"}
--     }
-- @
--
-- Note that @nameADT \@Thing "tag"@ is variadic: it accepts one
-- argument per constructor, except the nullary ones (Nullary) because
-- there's nothing to do for them.

-- $deconstruction
--
-- To deconstruct sum types at the SQL level, use 'deconstructADT',
-- which is also variadic, and has one argument for each
-- constructor. Similar to 'maybe'.
--
-- @
-- query :: Query (ADT Thing Expr)
-- query = do
--   thingExpr <- each thingSchema
--   where_ $
--     deconstructADT \@Thing
--       (\\employer -> employerName employer ==. lit \"Mary\")
--       (\\potato -> grower potato ==. lit \"Mary\")
--       (lit False) -- Nullary case
--       thingExpr
--   pure thingExpr
-- @
--
-- SQL output:
--
-- @
-- SELECT
-- CAST("tag0_1" AS text) as "tag",
-- CAST("id1_1" AS int4) as "ThingEmployer/_1/employerId",
-- CAST("name2_1" AS text) as "ThingEmployer/_1/employerName",
-- CAST("size3_1" AS int4) as "ThingPotato/_1/size",
-- CAST("Mary4_1" AS text) as "ThingPotato/_1/grower"
-- FROM (SELECT
--       *
--       FROM (SELECT
--             "tag" as "tag0_1",
--             "id" as "id1_1",
--             "name" as "name2_1",
--             "size" as "size3_1",
--             "Mary" as "Mary4_1"
--             FROM "thing" as "T1") as "T1"
--       WHERE (CASE WHEN ("tag0_1") = (CAST(E'ThingPotato' AS text)) THEN ("Mary4_1") = (CAST(E'Mary' AS text))
--                   WHEN ("tag0_1") = (CAST(E'Nullary' AS text)) THEN CAST(FALSE AS bool) ELSE ("name2_1") = (CAST(E'Mary' AS text)) END)) as "T1"
-- @

-- $construction
--
-- To construct an ADT, you can use 'buildADT' or 'constructADT'. Consider the following type:
--
-- @
-- data Task f = Pending | Complete (CompletedTask f)
-- @
--
-- 'buildADT' is for constructing values of 'Task' in the 'Expr'
-- context. 'buildADT' needs two type-level arguments before its type
-- makes any sense. The first argument is the type of the "ADT", which
-- in our case is 'Task'. The second is the name of the constructor we
-- want to use. So that means we have the following possible
-- instantiations of 'buildADT' for 'Task':
--
-- @
-- > :t buildADT \@Task \@\"Pending\"
-- buildADT \@Task \@\"Pending\" :: ADT Task Expr
-- > :t buildADT \@Task @\"Complete\"
-- buildADT \@Task \@\"Complete\" :: CompletedTask Expr -> ADT Task Expr
-- @
--
-- Note that as the "Pending" constructor has no fields, @buildADT
-- \@Task \@"Pending"@ is equivalent to @lit Pending@. But @buildADT
-- \@Task \@"Complete"@ is not the same as @lit . Complete@:
--
-- @
-- > :t lit . Complete
-- lit . Complete :: CompletedTask Result -> ADT Task Expr
-- @
--
--
-- Note that the former takes a @CompletedTask Expr@ while the latter
-- takes a @CompletedTask Result@. The former is more powerful because
-- you can construct @Task@s using dynamic values coming a database
-- query.
--
-- To show what this can look like in SQL, consider:
--
-- @
-- > :{
-- showQuery $ values
--   [ buildADT \@Task \@\"Pending\"
--   , buildADT \@Task \@\"Complete\" CompletedTask {date = Rel8.Expr.Time.now}
--   ]
-- :}
-- @
--
-- This produces the following SQL:
--
-- @
-- SELECT
-- CAST(\"values0_1\" AS text) as \"tag\",
-- CAST(\"values1_1\" AS timestamptz) as \"Complete/_1/date\"
-- FROM (SELECT
--       *
--       FROM (SELECT \"column1\" as \"values0_1\",
--                    \"column2\" as \"values1_1\"
--             FROM
--             (VALUES
--              (CAST(E'Pending' AS text),CAST(NULL AS timestamptz)),
--              (CAST(E'Complete' AS text),CAST(now() AS timestamptz))) as \"V\") as \"T1\") as \"T1\"
-- @
--
-- This is what you get if you run it in @psql@:
--
--
-- @
--    tag    |       Complete/_1/date
-- ----------+-------------------------------
--  Pending  |
--  Complete | 2022-05-19 21:28:23.969065+00
-- (2 rows)
-- @
--
-- "constructADT" is less convenient but more general alternative to
-- "buildADT". It requires only one type-level argument for its type
-- to make sense:
--
-- @
-- > :t constructADT @Task
-- constructADT @Task
--   :: (forall r. r -> (CompletedTask Expr -> r) -> r) -> ADT Task Expr
-- @
--
-- This might still seem a bit opaque, but basically it gives you a
-- Church-encoded constructor for arbitrary algebraic data types. You
-- might use it as follows:
--
-- @
-- let
--   pending :: ADT Task Expr
--   pending = constructADT \@Task $ \\pending _complete -> pending
--
--   complete :: ADT Task Expr
--   complete = constructADT \@Task $ \\_pending complete -> complete CompletedTask {date = Rel8.Expr.Time.now}
-- @
--
-- These values are otherwise identical to the ones we saw above with
-- @buildADT@, it's just a different style of constructing them.
--

-- $misc-notes
--
-- 1. Note that the order of the arguments for all of these functions
-- is determined by the order of the constructors in the data
-- definition. If it were @data Task = Complete (CompletedTask f) |
-- Pending@ then the order of all the invocations of @constructADT@
-- and @deconstructADT@ would need to change.
--
-- 2. Maybe this is obvious, but just to spell it out: once you're in
-- the @Result@ context, you can of course construct @Task@ values
-- normally and use standard Haskell pattern-matching. @constructADT@
-- and @deconstructADT@ are specifically only needed in the @Expr@
-- context, and they allow you to do the equivalent of pattern
-- matching in PostgreSQL.
