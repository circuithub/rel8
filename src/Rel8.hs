{-# language DuplicateRecordFields #-}

module Rel8
  ( -- * Getting Started
    -- $setup

    -- ** Writing Queries
    -- $guideQueries

    -- ** Joins
    -- $guideJoins

    -- ** Aggregation
    -- $guideAggregation

    -- * Database types
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
  , DBFractional

    -- * Tables and higher-kinded tables
  , Rel8able
  , Column
  , Default, Label
  , HMaybe
  , HList
  , HNonEmpty
  , HThese

  , Table
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
  , mapNullable
  , liftOpNullable
  , catNullable
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

  , Aggregates
  , sequenceAggregate
  , distributeAggregate


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
  , HasNullability
  , NotNull
  , HTable
  , Labelable
  , Context
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
import Rel8.Expr.Ord
import Rel8.Expr.Order
import Rel8.Expr.Serialize
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
import Rel8.Schema.Generic
import Rel8.Schema.HTable
import Rel8.Schema.Name
import Rel8.Schema.Nullability
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
import Rel8.Type.Eq
import Rel8.Type.Information
import Rel8.Type.JSONEncoded
import Rel8.Type.JSONBEncoded
import Rel8.Type.Monoid
import Rel8.Type.Num
import Rel8.Type.Ord
import Rel8.Type.ReadShow
import Rel8.Type.Semigroup
import Rel8.Type.String
import Rel8.Type.Sum
import Rel8.Expr.Opaleye (unsafeCastExpr)


-- $setup
--
-- In this section, we'll take a look at using Rel8 to work with a small
-- database for Haskell packages. We'll take a look at idiomatic usage of Rel8,
-- mapping tables to Haskell, and then look at writing some simple queries. 
--
-- The documentation in this library uses the following language extensions:
--
-- >>> :set -XBlockArguments 
-- >>> :set -XDeriveAnyClass 
-- >>> :set -XDeriveGeneric 
-- >>> :set -XDerivingStrategies 
-- >>> :set -XDerivingVia 
-- >>> :set -XDuplicateRecordFields
-- >>> :set -XGeneralizedNewtypeDeriving 
-- >>> :set -XOverloadedStrings
-- >>> :set -XStandaloneDeriving 
-- >>> :set -XTypeApplications 
-- >>> :set -XTypeFamilies
--
-- Before we start writing any Haskell, let's take a look at the schema we'll
-- work with.
--
-- > author:                               project:
-- > 
-- >   Column   │  Type   │ Nullable         Column   │  Type   │ Nullable 
-- > ═══════════╪═════════╪══════════      ═══════════╪═════════╪══════════
-- >  author_id │ integer │ not null        author_id │ integer │ not null 
-- >  name      │ text    │ not null        name      │ text    │ not null
-- >  url       │ text    │        
--
-- Our schema consists of two tables - @author@ and @project@. A @project@
-- always has an @author@, but not all @author@s have projects. Each @author@
-- has a name and (maybe) an associated website, and each project has a name.
--
-- Now that we've seen our schema, we can begin writing a mapping in Rel8. The
-- idiomatic way to map a table is to use a record that is parameterised what
-- Rel8 calls an /interpretation functor/, and to define each field with
-- 'Column'.  For this type to be usable with Rel8 we need it to be an instance
-- of 'HigherKindedTable', which can be derived with a combination of
-- @DeriveAnyClass@ and @DeriveGeneric@.
--
-- Following these steps for @author@, we have:
--
-- > data Author f = Author
-- >   { authorId :: Column f Int64
-- >   , name     :: Column f Text
-- >   , url      :: Column f (Maybe Text)
-- >   } deriving (Generic, HigherKindedTable)
--
-- This is a perfectly reasonable definition, but cautious readers might notice
-- a problem - in particular, with the type of the @authorId@ field.  While
-- @Int64@ is correct, it's not the best type. If we had other identifier types
-- in our project, it would be too easy to accidentally mix them up and create
-- nonsensical joins. As Haskell programmers, we often solve this problem by
-- creating @newtype@ wrappers, and we can also use this technique with Rel8:
--
-- >>> :{
-- newtype AuthorId = AuthorId { toInt64 :: Int64 } 
--   deriving newtype (DBEq, DBType, Eq, Show)
-- :}
--
-- Now we can write our final schema mapping. First, the @author@ table:
--
-- >>> :{
-- data Author f = Author
--   { authorId   :: Column f AuthorId
--   , authorName :: Column f Text
--   , authorUrl  :: Column f (Maybe Text)
--   } 
--   deriving stock Generic
--   deriving anyclass HigherKindedTable
-- :}
--
-- And similarly, the @project@ table:
--
-- >>> :{
-- data Project f = Project
--   { projectAuthorId :: Column f AuthorId
--   , projectName     :: Column f Text
--   } 
--   deriving stock Generic
--   deriving anyclass HigherKindedTable
-- :}
--
-- To show query results in this documentation, we'll also need @Show@
-- instances: Unfortunately these definitions look a bit scary, but they are
-- essentially just @deriving (Show)@:
--
-- >>> deriving stock instance f ~ Identity => Show (Author f)
-- >>> deriving stock instance f ~ Identity => Show (Project f)
--
-- These data types describe the structural mapping of the tables, but we also
-- need to specify a 'TableSchema' for each table. A @TableSchema@ contains the
-- name of the table and the name of all columns in the table, which will
-- ultimately allow us to @SELECT@ and @INSERT@ rows for these tables.
--
-- To define a @TableSchema@, we just need to fill construct appropriate
-- @TableSchema@ values. When it comes to the @tableColumns@ field, we
-- construct values of our data types above, and set each field to the name of
-- the column that it maps to:
--
-- First, @authorSchema@ describes the column names of the @author@ table when
-- associated with the @Author@ type:
--
-- >>> :{
-- authorSchema :: TableSchema (Author ColumnSchema)
-- authorSchema = TableSchema
--   { tableName = "author"
--   , tableSchema = Nothing
--   , tableColumns = Author
--       { authorId = "author_id"
--       , authorName = "name"
--       , authorUrl = "url"
--       }
--   }
-- :}
--
-- And likewise for @project@ and @Project@:
--
-- >>> :{
-- projectSchema :: TableSchema (Project ColumnSchema)
-- projectSchema = TableSchema
--   { tableName = "project"
--   , tableSchema = Nothing
--   , tableColumns = Project
--       { projectAuthorId = "author_id"
--       , projectName = "name"
--       }
--   }
-- :}
--
-- Aside: you might be wondering why this information isn't in the definitions
-- of @Author@ and @Project@ above. Rel8 decouples @TableSchema@ from the data
-- types themselves, as not all tables you define will necessarily have a
-- schema. For example, Rel8 allows you to define helper types to simplify the
-- types of queries - these tables only exist at query time, but there is no
-- corresponding base table. We'll see more on this idea later!
--
-- With these table definitions, we can now start writing some queries! To
-- actually run a query, we'll need a database connection. Rel8 uses the
-- @hasql@ library, and for this documentation we'll get the connection string
-- from the @$TEST_DATABASE_URL@ environment variable.
--
-- >>> connectionString <- System.Environment.getEnv "TEST_DATABASE_URL"
-- >>> Right c <- Hasql.Connection.acquire (Data.ByteString.Char8.pack connectionString)
-- >>> Control.Monad.void $ Hasql.Session.run (Hasql.Session.sql "BEGIN") c

-- $guideQueries
-- 
-- First, we'll take a look at @SELECT@ statements - usually the bulk of most
-- database heavy applications.
-- 
-- In Rel8, @SELECT@ statements are built using the 'Query' monad. You can
-- think of this monad like the ordinary @[]@ (List) monad - but this isn't
-- required knowledge. 
-- 
-- To start, we'll look at one of the simplest queries possible - a basic
-- @SELECT * FROM@ statement. To select all rows from a table, we use 'each',
-- and supply a 'TableSchema'. So to select all @project@ rows, we can write:
-- 
-- >>> :t each projectSchema
-- each projectSchema :: Query (Project Expr)
-- 
-- Notice that @each@ gives us a @Query@ that yields @Project Expr@ rows. To
-- see what this means, let's have a look at a single field of a 
-- @Project Expr@:
-- 
-- >>> let aProjectExpr = undefined :: Project Expr
-- >>> :t projectAuthorId aProjectExpr
-- projectAuthorId aProjectExpr :: Expr AuthorId
-- 
-- Recall we defined @projectAuthorId@ as @Column f AuthorId@. Now we have @f@
-- is @Expr@, and @Column Expr AuthorId@ reduces to @Expr AuthorId@. We'll see
-- more about @Expr@ soon, but you can think of @Expr a@ as "SQL expressions of
-- type @a@".
-- 
-- To execute this @Query@, we pass it to 'select':
-- 
-- >>> :t select c (each projectSchema)
-- select c (each projectSchema) :: MonadIO m => m [Project Identity]
-- 
-- When we @select@ things containing @Expr@s, Rel8 builds a new response table
-- with the @Identity@ interpretation. This means you'll get back plain Haskell
-- values. Studying @projectAuthorId@ again, we have:
-- 
-- >>> let aProjectIdentity = undefined :: Project Identity
-- >>> :t projectAuthorId aProjectIdentity
-- projectAuthorId aProjectIdentity :: AuthorId
-- 
-- Here @Column Identity AuthorId@ reduces to just @AuthorId@, with no
-- wrappping type at all.
-- 
-- Putting this all together, we can run our first query:
-- 
-- >>> select c (each projectSchema) >>= mapM_ print
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
-- 
-- Cool!
--
-- We now know that 'each' is the equivalent of a @SELECT *@ query, but
-- sometimes we're only interested in a subset of the columns of a table. To
-- restrict the returned columns, we can specify a projection by using 'Query's
-- @Functor@ instance:
--
-- >>> select c $ projectName <$> each projectSchema
-- ["rel8","aeson","text"]

-- $guideJoins
-- 
-- Another common operation in relational databases is to take the @JOIN@ of
-- multiple tables. Rel8 doesn't have a specific join operation, but we can
-- recover the functionality of a join by selecting all rows of two tables, and
-- then using 'where_' to filter them.
--
-- To see how this works, first let's look at taking the product of two tables.
-- We can do this by simply calling 'each' twice, and then returning a tuple of
-- their results.
--
-- >>> :{
-- mapM_ print =<< select c do
--   author  <- each authorSchema
--   project <- each projectSchema
--   return (projectName project, authorName author)
-- :}
-- ("rel8","Ollie")
-- ("rel8","Bryan O'Sullivan")
-- ("rel8","Emily Pillmore")
-- ("aeson","Ollie")
-- ("aeson","Bryan O'Sullivan")
-- ("aeson","Emily Pillmore")
-- ("text","Ollie")
-- ("text","Bryan O'Sullivan")
-- ("text","Emily Pillmore")
--
-- This isn't quite right, though, as we have ended up pairing up the wrong
-- projects and authors. To fix this, we can use 'where_' to restrict the
-- returned rows. We could write:
-- 
-- > select c $ do
-- >   author  <- each authorSchema
-- >   project <- each projectSchema
-- >   where_ $ projectAuthorId project ==. authorId author
-- >   return (project, author)
--
-- but doing this every time you need a join can obscure the meaning of the
-- query you're writing. A good practice is to introduce specialised functions
-- for the particular joins in your database. In our case, this would be:
--
-- >>> :{
-- projectsForAuthor :: Author Expr -> Query (Project Expr)
-- projectsForAuthor a = each projectSchema >>= filter \p ->
--   projectAuthorId p ==. authorId a
-- :}
--
-- Our final query is then:
--
-- >>> :{
-- mapM_ print =<< select c do
--   author  <- each authorSchema
--   project <- projectsForAuthor author
--   return (projectName project, authorName author)
-- :}
-- ("rel8","Ollie")
-- ("aeson","Bryan O'Sullivan")
-- ("text","Bryan O'Sullivan")
--
-- == @LEFT JOIN@s
--
-- Rel8 is also capable of performing @LEFT JOIN@s. To perform @LEFT JOIN@s, we
-- follow the same approach as before, but use the 'optional' query transformer
-- to allow for the possibility of the join to fail.
--
-- In our test database, we can see that there's another author who we haven't
-- seen yet:
--
-- >>> select c $ authorName <$> each authorSchema
-- ["Ollie","Bryan O'Sullivan","Emily Pillmore"]
--
-- Emily wasn't returned in our earlier query because - in our database - she
-- doesn't have any registered projects. We can account for this partiality in
-- our original query by wrapping the @projectsForAuthor@ call with 'optional':
--
-- >>> :{
-- mapM_ print =<< select c do
--   author   <- each authorSchema
--   mproject <- optional $ projectsForAuthor author
--   return (authorName author, projectName <$> mproject)
-- :}
-- ("Ollie",Just "rel8")
-- ("Bryan O'Sullivan",Just "aeson")
-- ("Bryan O'Sullivan",Just "text")
-- ("Emily Pillmore",Nothing)

-- $guideAggregation
--
-- Aggregations are operations like @sum@ and @count@ - operations that reduce
-- multiple rows to single values. To perform aggregations in Rel8, we can use
-- the 'aggregate' function, which takes a 'Query' of aggregated expressions,
-- runs the aggregation, and returns aggregated rows.
--
-- To start, let's look at a simple aggregation that tells us how many projects
-- exist:
--
-- >>> error "TODO"
--
-- Rel8 is also capable of aggregating multiple rows into a single row by
-- concatenating all rows as a list. This aggregation allows us to break free
-- of the row-orientated nature of SQL and write queries that return tree-like
-- structures. Earlier we saw an example of returning authors with their
-- projects, but the query didn't do a great job of describing the one-to-many
-- relationship between authors and their projects.
--
-- Let's look again at a query that returns authors and their projects, and
-- focus on the /type/ of that query. 
--
-- >>> :{
-- projectsForAuthor a = each projectSchema >>= filter \p ->
--   projectAuthorId p ==. authorId a
-- :}
--
-- >>> :{
-- let authorsAndProjects = do
--       author  <- each authorSchema
--       project <- projectsForAuthor author
--       return (author, project)
--       where
-- :}
--
-- >>> :t select c authorsAndProjects
-- select c authorsAndProjects 
--   :: MonadIO m => m [(Author Identity, Project Identity)]
--
-- Our query gives us a single list of pairs of authors and projects. However,
-- with our domain knowledge of the schema, this isn't a great type - what we'd
-- rather have is a list of pairs of authors and /lists/ of projects. That is,
-- what we'd like is:
--
-- > [(Author Identity, [Project Identity])]
--
-- This would be a much better type! Rel8 can produce a query with this type by
-- simply wrapping the call to @projectsForAuthor@ with either 'some' or
-- 'many'. Here we'll use 'many', which allows for the possibility of an author
-- to have no projects:
--
-- >>> :{
-- mapM_ print =<< select c do
--   author       <- each authorSchema
--   projectNames <- many $ projectName <$> projectsForAuthor author
--   return (authorName author, projectNames)
-- :}
-- ("Ollie",["rel8"])
-- ("Bryan O'Sullivan",["aeson","text"])
-- ("Emily Pillmore",[])
