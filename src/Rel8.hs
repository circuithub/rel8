{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language InstanceSigs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

{-# options -Wno-deprecations #-}

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
    New.DBType(..)

    -- *** Deriving-via helpers
    -- **** @JSONEncoded@
  , JSONEncoded(..)
  , JSONBEncoded(..)

    -- **** @ReadShow@
  , ReadShow(..)

    -- *** @TypeInformation@
  , New.TypeInformation(..)
  , New.mapTypeInformation
  , New.parseTypeInformation

    -- ** @DBEq@
  , New.DBEq

    -- * Tables and higher-kinded tables
  , New.Table(..)
  , New.Rel8able
  -- , HTable
  -- , HigherKindedTable
  -- , Congruent
  -- , KContext
  -- , Context

    -- ** Table schemas
  , New.Column
  , TableSchema(..)
  -- , ColumnSchema

    -- * Expressions
  , Expr
  -- , unsafeCastExpr
  -- , unsafeCoerceExpr
  -- , unsafeLiteral
  -- , binaryOperator

    -- ** @null@
  , NullExpr
  , nullExpr
  , null
  , isNull
  , liftNull
  , mapNull
  , liftOpNull
  , catMaybe
  , fromNull

    -- ** Boolean operations
  , (&&.)
  , and_
  , (||.)
  , or_
  , not_
  , in_
  , ifThenElse_
  -- , EqTable(..)
  , case_

    -- ** Ordering
  , New.DBOrd

    -- ** Functions
  , Function
  , function
  , nullaryFunction

    -- * Queries
  , Query
  , showQuery

    -- ** Selecting rows
  -- , Selects
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

    -- ** Optional 'Query's
  , New.MaybeTable
  , HMaybe
  , optional
  , maybeTable
  , isNothingTable
  , New.nothingTable
  , New.justTable
  , catMaybeTable
  , exists
  , bindMaybeTable
  , traverseMaybeTable

    -- ** Aggregation
  , Aggregate
  , aggregate
  , listAgg
  , nonEmptyAgg
  , groupBy
  , DBMax( max )
  , DBMin( min )
  , DBSum( sum )
  , count
  , countStar
  , countDistinct
  , countRows
  , boolOr
  , boolAnd

    -- *** List aggregation
  , New.ListTable
  -- , HList
  , many
  , New.NonEmptyTable
  -- , HNonEmpty
  , some

    -- ** Ordering
  , orderBy
  , Order
  , asc
  , desc
  , nullsFirst
  , nullsLast

    -- * IO
  , New.Serializable
  -- , ExprFor

    -- * Running statements
    -- ** @SELECT@
  , select

  --   -- ** @INSERT@
  -- , Insert(..)
  -- , OnConflict(..)
  -- , insert
  -- , defaultValue

  --   -- ** @DELETE@
  -- , Delete(..)
  -- , delete

  --   -- ** @UPDATE@
  -- , update
  -- , Update(..)

  --   -- ** @.. RETURNING@
  -- , Returning(..)
  ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- base
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Bifunctor ( first )
import Data.Foldable ( fold, foldl', toList )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Contravariant ( Contravariant )
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Type )
import Data.String ( IsString(..) )
import Data.Typeable ( Typeable )
import Numeric.Natural ( Natural )
import Prelude hiding ( filter, max, null )
import Text.Read ( readEither )

-- contravariant
import Data.Functor.Contravariant.Divisible ( Decidable, Divisible )

-- opaleye
import qualified Opaleye ( OnConflict(..), PGInt8, valuesExplicit )
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Binary as Opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.Order as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye hiding ( BinOp, aggregate, limit, exists )
import qualified Opaleye.Internal.Print as Opaleye ( formatAndShowSQL )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Exists as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Lateral as Opaleye
import qualified Opaleye.Operators as Opaleye hiding ( exists, restrict )
import qualified Opaleye.Order as Opaleye (limit, offset, orderBy)
import Opaleye.PGTypes
  ( IsSqlType(..)
  , pgBool
  , pgInt4
  , pgInt8
  , pgNumeric
  , pgStrictText
  , pgValueJSON, pgUTCTime, pgDay, pgLocalTime, pgTimeOfDay, pgUUID, pgStrictByteString, pgDouble
  )
import qualified Opaleye.Table as Opaleye

-- rel8
-- import qualified Rel8.Optimize

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text, pack )
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )

-- transformers
import Data.Time (UTCTime, Day, LocalTime, TimeOfDay)
import Data.UUID (UUID)
import qualified Data.Text.Lazy
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Hasql.Decoders as Hasql
import Hasql.Connection (Connection)
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Hasql (noParams)
import Control.Exception (throwIO)

import qualified Rel8.Expr as New
import qualified Rel8.Expr.Bool as New
import qualified Rel8.Expr.Eq as New
import qualified Rel8.Expr.Null as New
import qualified Rel8.Expr.Opaleye as New
import qualified Rel8.Kind.Nullability as New
import qualified Rel8.Schema.Column as New
import qualified Rel8.Schema.Context as New
import qualified Rel8.Schema.Generic as New
import qualified Rel8.Schema.HTable as New
import qualified Rel8.Schema.HTable.Vectorize as New
import qualified Rel8.Table as New
import qualified Rel8.Table.Bool as New
import qualified Rel8.Table.List as New
import qualified Rel8.Table.Maybe as New
import qualified Rel8.Table.NonEmpty as New
import qualified Rel8.Table.Opaleye as New
import qualified Rel8.Table.Recontextualize as New
import qualified Rel8.Table.Serialize as New
import qualified Rel8.Table.Serialize as New
import qualified Rel8.Type as New
import qualified Rel8.Type.Eq as New
import qualified Rel8.Type.Monoid as New
import qualified Rel8.Type.Num as New
import qualified Rel8.Type.Ord as New
import qualified Rel8.Type.Semigroup as New
import qualified Rel8.Type.Tag as New


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



-- | Haskell types that can be represented as expressions in a database. There
-- should be an instance of @DBType@ for all column types in your database
-- schema (e.g., @int@, @timestamptz@, etc).
-- 
-- Rel8 comes with stock instances for all default types in PostgreSQL, so you
-- should only need to derive instances of this class for custom database
-- types, such as types defined in PostgreSQL extensions, or custom domain
-- types.
-- 
-- [ Creating @DBType@s using @newtype@ ]
-- 
-- Generalized newtype deriving can be used when you want use a @newtype@
-- around a database type for clarity and accuracy in your Haskell code. A
-- common example is to @newtype@ row id types:
-- 
-- >>> newtype UserId = UserId { toInt32 :: Int32 } deriving newtype (DBType)
-- 
-- You can now write queries using @UserId@ instead of @Int32@, which may help
-- avoid making bad joins. However, when SQL is generated, it will be as if you
-- just used integers (the type distinction does not impact query generation).
-- class Typeable a => DBType (a :: Type) where
--   -- | Lookup the type information for the type @a@.
--   typeInformation :: DatabaseType a


-- | A deriving-via helper type for column types that store a Haskell value
-- using a JSON encoding described by @aeson@'s 'ToJSON' and 'FromJSON' type
-- classes.
-- 
-- The declaration:
-- 
-- >>> import Data.Aeson
-- 
-- >>> :{
-- data Pet = Pet { petName :: String, petAge :: Int }
--   deriving (Generic, ToJSON, FromJSON)
--   deriving DBType via JSONEncoded Pet
-- :}
-- 
-- will allow you to store @Pet@ values in a single SQL column (stored as
-- @json@ values).
newtype JSONEncoded a = JSONEncoded { fromJSONEncoded :: a }


instance (FromJSON a, ToJSON a, Typeable a) => New.DBType (JSONEncoded a) where
  typeInformation = New.parseTypeInformation f g New.typeInformation
    where
      f = fmap JSONEncoded . parseEither parseJSON
      g = toJSON . fromJSONEncoded


-- | Like 'JSONEncoded', but works for @jsonb@ columns.
newtype JSONBEncoded a = JSONBEncoded { fromJSONBEncoded :: a }


instance (FromJSON a, ToJSON a, Typeable a) => New.DBType (JSONBEncoded a) where
  typeInformation = New.parseTypeInformation f g New.TypeInformation
    { encode = New.encode New.typeInformation
    , decode = Hasql.jsonb
    , typeName = "jsonb"
    }
    where
      f = fmap JSONBEncoded . parseEither parseJSON
      g = toJSON . fromJSONBEncoded


-- | A deriving-via helper type for column types that store a Haskell value
-- using a Haskell's 'Read' and 'Show' type classes.
--
-- The declaration:
-- 
-- >>> :{
-- data Color = Red | Green | Blue
--   deriving (Read, Show)
--   deriving DBType via ReadShow Color
-- :}
-- 
-- will allow you to store @Color@ values in a single SQL column (stored as
-- @text@).
newtype ReadShow a = ReadShow { fromReadShow :: a }


-- | Typed SQL expressions. These SQL expressions prohibit @null@. If you want
-- to model the ability for expression to be @null@, see 'NullExpr'.
newtype Expr (a :: Type) = Expr { fromExpr :: New.Expr 'New.NonNullable a }


instance New.Table (New.Expr 'New.NonNullable a) => New.Table (Expr a) where
  type Context (Expr a) = New.Context (New.Expr 'New.NonNullable a)
  type Columns (Expr a) = New.Columns (New.Expr 'New.NonNullable a)
  toColumns = New.toColumns . fromExpr
  fromColumns = Expr . New.fromColumns


liftExpr :: (New.Expr 'New.NonNullable a -> New.Expr 'New.NonNullable b) -> Expr a -> Expr b
liftExpr f (Expr a) = Expr (f a)


zipExprWith 
  :: (New.Expr 'New.NonNullable a -> New.Expr 'New.NonNullable b -> New.Expr 'New.NonNullable c) 
  -> Expr a -> Expr b -> Expr c
zipExprWith f (Expr a) (Expr b) = Expr (f a b)


instance New.DBNum a => Num (Expr a) where
  (+) = zipExprWith (+)
  (-) = zipExprWith (-)
  (*) = zipExprWith (*)
  abs = liftExpr abs
  signum = liftExpr signum
  fromInteger = Expr . fromInteger
  negate = liftExpr negate


instance (New.DBType a, IsString a) => IsString (Expr a) where
  fromString = Expr . fromString


instance New.DBFractional a => Fractional (Expr a) where
  (/) = zipExprWith (/)
  fromRational = Expr  . fromRational


instance New.DBSemigroup a => Semigroup (Expr a) where
  (<>) = zipExprWith (<>)


instance New.DBMonoid a => Monoid (Expr a) where
  mempty = Expr mempty


newtype NullExpr (a :: Type) = NullExpr { fromNullExpr :: New.Expr 'New.Nullable a }


instance New.Table (New.Expr 'New.Nullable a) => New.Table (NullExpr a) where
  type Context (NullExpr a) = New.Context (New.Expr 'New.Nullable a)
  type Columns (NullExpr a) = New.Columns (New.Expr 'New.Nullable a)
  toColumns = New.toColumns . fromNullExpr
  fromColumns = NullExpr . New.fromColumns


liftNullExpr :: (New.Expr 'New.Nullable a -> New.Expr 'New.Nullable b) -> NullExpr a -> NullExpr b
liftNullExpr f (NullExpr a) = NullExpr (f a)


zipNullExprWith 
  :: (New.Expr 'New.Nullable a -> New.Expr 'New.Nullable b -> New.Expr 'New.Nullable c) 
  -> NullExpr a -> NullExpr b -> NullExpr c
zipNullExprWith f (NullExpr a) (NullExpr b) = NullExpr (f a b)


instance New.DBNum a => Num (NullExpr a) where
  (+) = zipNullExprWith (+)
  (-) = zipNullExprWith (-)
  (*) = zipNullExprWith (*)
  abs = liftNullExpr abs
  signum = liftNullExpr signum
  fromInteger = NullExpr . fromInteger
  negate = liftNullExpr negate


instance (New.DBType a, IsString a) => IsString (NullExpr a) where
  fromString = NullExpr . fromString


instance New.DBFractional a => Fractional (NullExpr a) where
  (/) = zipNullExprWith (/)
  fromRational = NullExpr . fromRational


instance New.DBSemigroup a => Semigroup (NullExpr a) where
  (<>) = zipNullExprWith (<>)


instance New.DBMonoid a => Monoid (NullExpr a) where
  mempty = NullExpr mempty



---- | Cast an @Expr@ from one type to another.
--unsafeCastExpr :: String -> Expr a -> Expr b
--unsafeCastExpr t (Expr x) = Expr $ Opaleye.CastExpr t x


---- | Unsafely treat an 'Expr' that returns @a@s as returning @b@s.
--unsafeCoerceExpr :: Expr a -> Expr b
--unsafeCoerceExpr (Expr x) = Expr x


---- | Construct a SQL expression from some literal text. The provided literal
---- will be interpolated exactly as specified with no escaping.
--unsafeLiteral :: forall a. String -> Expr a
--unsafeLiteral = columnToExpr @a @a . Opaleye.Column . Opaleye.ConstExpr . Opaleye.OtherLit


---- | Construct an expression by applying an infix binary operator to two
---- operands.
--binaryOperator :: String -> Expr a -> Expr b -> Expr c
--binaryOperator op (Expr a) (Expr b) = Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b


-- | Like 'maybe', but to eliminate 'NullExpr'.
--
-- >>> select c $ pure $ null 0 id (nullExpr :: Expr (Maybe Int32))
-- [0]
--
-- >>> select c $ pure $ null 0 id (lit (Just 42) :: Expr (Maybe Int32))
-- [42]
null :: New.DBType b => Expr b -> (Expr a -> Expr b) -> NullExpr a -> Expr b
null (Expr def) f (NullExpr a) = Expr $ New.nullable def (fromExpr . f . Expr) a


-- | Like 'isNothing', but for @null@.
--
-- >>> select c $ pure $ isNull (nullExpr :: Expr (Maybe Int32))
-- [True]
--
-- >>> select c $ pure $ isNull (lit (Just 42) :: Expr (Maybe Int32))
-- [False]
isNull :: NullExpr a -> Expr Bool
isNull = Expr . New.isNull  . fromNullExpr


-- | Corresponds to SQL @null@.
nullExpr :: New.DBType a => NullExpr a
nullExpr = NullExpr New.null


-- | Lift an expression that's not null to a type that might be @null@. This is
-- an identity operation in terms of any generated query, and just modifies the
-- query's type.
liftNull :: Expr a -> NullExpr a
liftNull (Expr a) = NullExpr $ New.nullify a


-- | Lift an operation on non-@null@ values to an operation on possibly @null@
-- values.
-- 
-- @mapNull@ requires that the supplied function "preserves nulls", as no
-- actual case analysis is done (instead the @Expr (Maybe a)@ is simply retyped
-- and assumed to not be @null@). In most cases, this is true, but this
-- contract can be violated with custom functions.
mapNull :: New.DBType b => (Expr a -> Expr b) -> NullExpr a -> NullExpr b
mapNull f = NullExpr . New.mapNullable (fromExpr . f . Expr) . fromNullExpr 


fromNull :: New.DBType a => Expr a -> NullExpr a -> Expr a
fromNull (Expr def) (NullExpr x) = Expr $ New.nullable def id x


-- | Lift a binary operation on non-@null@ expressions to an equivalent binary
-- operator on possibly @null@ expressions.
-- 
-- Similar to @mapNull@, it is assumed that this binary operator will return
-- @null@ if either of its operands are @null@.
-- 
-- >>> select c $ pure $ liftOpNull (&&.) (lit (Just True)) (lit (Just False))
-- [Just False]
-- 
-- >>> select c $ pure $ liftOpNull (&&.) nullExpr (lit (Just False))
-- [Nothing]
-- 
-- This function can be thought of like 'liftA2'.
liftOpNull :: New.DBType c => (Expr a -> Expr b -> Expr c) -> NullExpr a -> NullExpr b -> NullExpr c
liftOpNull f (NullExpr x) (NullExpr y) = NullExpr $ New.liftOpNullable go x y
  where
    go x' y' = fromExpr $ f (Expr x') (Expr y')


-- | Filter a 'Query' that might return @null@ to a 'Query' without any
-- @null@s.
--
-- Corresponds to 'Data.Maybe.catMaybes'.
-- 
-- >>> select c $ pure (nullExpr :: Expr (Maybe Bool))
-- [Nothing]
-- 
-- >>> select c $ catMaybe (nullExpr :: Expr (Maybe Bool))
-- []
-- 
-- >>> select c $ catMaybe (lit (Just True))
-- [True]
-- 
-- Notice how in the last example a @Bool@ is returned (rather than @Maybe
-- Bool@):
-- 
-- >>> :t catMaybe (lit (Just True))
-- catMaybe (lit (Just True)) :: Query (Expr Bool)
catMaybe :: NullExpr a -> Query (Expr a)
catMaybe e = catMaybeTable $ New.MaybeTable nullTag (Expr (New.unsafeUnnullify (fromNullExpr e)))
  where
    nullTag = ifThenElse_ (isNull e) (New.lit Nothing) (New.lit (Just New.IsJust))


-- | The SQL @AND@ operator.
--
-- >>> :{
-- mapM_ print =<< select c do
--   x <- values [lit True, lit False]
--   y <- values [lit True, lit False]
--   return (x, y, x &&. y)
-- :}
-- (True,True,True)
-- (True,False,False)
-- (False,True,False)
-- (False,False,False)
infixr 3 &&.


(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
Expr a &&. Expr b = Expr $ a New.&&. b


-- | Fold @AND@ over a collection of expressions.
--  
-- >>> select c $ pure $ and_ [ lit True ==. lit False, lit False, lit True ]
-- [False]
--  
-- >>> select c $ pure $ and_ []
-- [True]
and_ :: Foldable f => f (Expr Bool) -> Expr Bool
and_ = foldl' (&&.) (Expr $ New.lit True)


-- | The SQL @OR@ operator.
--
-- >>> :{
-- mapM_ print =<< select c do
--   x <- values [lit True, lit False]
--   y <- values [lit True, lit False]
--   return (x, y, x ||. y)
-- :}
-- (True,True,True)
-- (True,False,True)
-- (False,True,True)
-- (False,False,False)
infixr 2 ||.


(||.) :: Expr Bool -> Expr Bool -> Expr Bool
Expr a ||. Expr b = Expr $ a New.||. b


-- | Fold @OR@ over a collection of expressions.
-- 
-- >>> select c $ pure $ or_ [ lit True ==. lit False, lit False, lit True ]
-- [True]
--  
-- >>> select c $ pure $ or_ []
-- [False]
or_ :: Foldable f => f (Expr Bool) -> Expr Bool
or_ = foldl' (||.) (Expr $ New.lit False)


-- | The SQL @NOT@ operator.
--
-- >>> select c $ pure $ not_ $ lit True
-- [False]
--
-- >>> select c $ pure $ not_ $ lit False
-- [True]
not_ :: Expr Bool -> Expr Bool
not_ (Expr a) = Expr $ New.not_ a


-- | Like the SQL @IN@ operator, but implemented by folding over a list with
-- '==.' and '||.'.
--
-- >>> select c $ return $ lit (5 :: Int32) `in_` [ lit x | x <- [1..5] ]
-- [True]
--
-- >>> select c $ return $ lit (42 :: Int32) `in_` [ lit x | x <- [1..5] ]
-- [False]
in_ :: New.DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> b ||. x ==. y) (Expr $ New.lit False)


-- | Branch two expressions based on a predicate. Similar to @if ... then ...
-- else@ in Haskell (and implemented using @CASE@ in SQL). 
-- 
-- >>> select c (return (ifThenElse_ (lit True) (lit "True!") (lit "False!") :: Expr Text))
-- ["True!"]
-- 
-- Note that unlike SQL, this function can be used to return multiple columns:
-- 
-- >>> import Data.Text (pack)
-- >>> :{
-- select c $ pure $
--   ifThenElse_ 
--     (lit False) 
--     (lit (pack "A", pack "B")) 
--     (lit (pack "C", pack "D"))
-- :}
-- [("C","D")]
ifThenElse_ :: (New.Table a, New.Context a ~ New.DB) => Expr Bool -> a -> a -> a
ifThenElse_ (Expr b) true false = New.bool false true b


-- | The @Function@ type class is an implementation detail that allows
-- @function@ to be polymorphic in the number of arguments it consumes.
class Function arg res where
  -- | Build a function of multiple arguments.
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res
  applyArgument =
    -- We do need 'applyArgument', but if we don't specify this and let GHC
    -- infer the minimal contract, it leaks out into documentation. This is a
    -- private class, so we don't want to document anything more than its
    -- existance.
    undefined


instance arg ~ Expr a => Function arg (Expr res) where
  applyArgument mkExpr (Expr a) = Expr $ New.fromPrimExpr $ mkExpr [New.toPrimExpr a]


instance (arg ~ Expr a, Function args res) => Function arg (args -> res) where
  applyArgument f (Expr a) = applyArgument (f . (New.toPrimExpr a :))


-- | Construct an n-ary function that produces an 'Expr' that when called runs
-- a SQL function.
-- 
-- For example, here's how we can wrap PostgreSQL's @factorial@ function:
-- 
-- >>> :{
-- factorial :: Expr Int64 -> Expr Data.Scientific.Scientific
-- factorial = function "factorial"
-- :}
-- 
-- >>> select c $ pure $ factorial 5
-- [120.0]
-- 
-- The same approach works for any number of arguments:
-- 
-- >>> :{
-- power :: Expr Float -> Expr Float -> Expr Double
-- power = function "power"
-- :}
-- 
-- >>> select c $ pure $ power 9 3
-- [729.0]
function :: Function args result => String -> args -> result
function = applyArgument . Opaleye.FunExpr


-- | Construct a function call for functions with no arguments.
-- 
-- For example, we can call the database function @pi()@ by using
-- @nullaryFunction@:
-- 
-- >>> :{
-- sqlPi :: Expr Double
-- sqlPi = nullaryFunction "pi"
-- :}
-- 
-- >>> select c $ pure $ sqlPi
-- [3.141592653589793]
nullaryFunction :: String -> Expr a
nullaryFunction name = Expr $ New.fromPrimExpr $ Opaleye.FunExpr name []


-- | The schema for a table. This is used to specify the name and schema that a
-- table belongs to (the @FROM@ part of a SQL query), along with the schema of
-- the columns within this table.
-- 
-- For each selectable table in your database, you should provide a
-- @TableSchema@ in order to interact with the table via Rel8. For a table
-- storing a list of projects (as defined in the introduction):
-- 
-- >>> :{
-- projectSchema :: TableSchema (Project ColumnSchema)
-- projectSchema = TableSchema
--   { tableName = "project"
--   , tableSchema = Nothing -- Assumes that the 'project' table is reachable from your connection's search_path
--   , tableColumns = Project 
--       { projectAuthorId = "author_id"
--       , projectName = "name"
--       }
--   }
-- :}
data TableSchema (schema :: Type) = TableSchema
  { tableName :: String
    -- ^ The name of the table.
  , tableSchema :: Maybe String
    -- ^ The schema that this table belongs to. If 'Nothing', whatever is on
    -- the connection's @search_path@ will be used.
  , tableColumns :: schema
    -- ^ The columns of the table. Typically you would use a a higher-kinded
    -- data type here, parameterized by the 'Rel8.ColumnSchema.ColumnSchema' functor.
  } deriving stock Functor


-- | Perform case analysis on a 'MaybeTable'. Like 'maybe'.
maybeTable
  :: (New.Context b ~ New.DB, New.Table b)
  => b -> (a -> b) -> New.MaybeTable a -> b
maybeTable = New.maybeTable


isNothingTable :: New.MaybeTable a -> Expr Bool
isNothingTable = Expr . New.isNothingTable


-- | @bindMaybeTable f x@ is similar to the monadic bind (@>>=@) operation. It
-- allows you to "extend" an optional query with another query. If either the
-- input or output are 'noTable', then the result is 'noTable'.
--
-- This is similar to 'traverseMaybeTable', followed by a @join@ on the
-- resulting @MaybeTable@s.
--
-- >>> select c $ bindMaybeTable (optional . values . pure . not_) =<< optional (values [lit True])
-- [Just False]
--
-- >>> select c $ bindMaybeTable (\_ -> return (noTable :: MaybeTable (Expr Bool))) =<< optional (values [lit True])
-- [Nothing]
--
-- >>> select c $ bindMaybeTable (optional . values . pure . not_) =<< return (noTable :: MaybeTable (Expr Bool))
-- [Nothing]
bindMaybeTable :: (a -> Query (New.MaybeTable b)) -> New.MaybeTable a -> Query (New.MaybeTable b)
bindMaybeTable query New.MaybeTable{ tag, just } = do
  New.MaybeTable tag' just' <- query just
  return $ New.MaybeTable{ tag = tag <> tag', just = just' }


-- | Extend an optional query with another query.  This is useful if you want
-- to step through multiple @LEFT JOINs@.
--
-- Note that @traverseMaybeTable@ takes a @a -> Query b@ function, which means
-- you also have the ability to "expand" one row into multiple rows.
--
-- >>> :{
-- duplicate :: Expr Bool -> Query (Expr Bool)
-- duplicate x = unionAll (return x) (return x)
-- :}
--
-- >>> select c $ traverseMaybeTable duplicate =<< optional (values [lit True])
-- [Just True,Just True]
--
-- Note that if the @a -> Query b@ function returns no rows, then the resulting
-- query will also have no rows:
--
-- >>> select c $ traverseMaybeTable (limit 0 . pure) =<< optional (values [lit True])
-- []
--
-- However, regardless of the given @a -> Query b@ function, if the input is
-- @noTable@, you will always get exactly one @noTable@ back:
--
-- >>> select c $ traverseMaybeTable duplicate (noTable :: MaybeTable (Expr Bool))
-- [Nothing]
--
-- >>> select c $ traverseMaybeTable (limit 0 . pure) (noTable :: MaybeTable (Expr Bool))
-- [Nothing]
traverseMaybeTable :: (a -> Query b) -> New.MaybeTable a -> Query (New.MaybeTable b)
traverseMaybeTable query ma@(New.MaybeTable input _) = do
  New.MaybeTable output b <- optional (query =<< catMaybeTable ma)
  where_ $ isNull (NullExpr output) ==. isNull (NullExpr input)
  return $ New.MaybeTable input b


case_ :: (New.Context a ~ New.DB, New.Table a) => [(Expr Bool, a)] -> a -> a
case_ alts = New.case_ (map (first fromExpr) alts)


-- | The 'DBType' instance for 'ReadShow' allows you to serialize a type using
-- Haskell's 'Read' and 'Show' instances:
--
-- @
-- data Color = Red | Green | Blue
--   deriving (Read, Show)
--   deriving DBType via ReadShow Color
-- @
instance (Read a, Show a, Typeable a) => New.DBType (ReadShow a) where
  typeInformation = New.parseTypeInformation parser printer New.typeInformation
    where
      parser = fmap ReadShow . readEither . Text.unpack
      printer = Text.pack . show . fromReadShow


-- | The type of @SELECT@able queries. You generally will not explicitly use
-- this type, instead preferring to be polymorphic over any 'MonadQuery m'.
-- Functions like 'select' will instantiate @m@ to be 'Query' when they run
-- queries.
newtype Query a = Query (Opaleye.Query a)
  deriving newtype (Functor, Applicative)


liftOpaleye :: Opaleye.Query a -> Query a
liftOpaleye = Query


toOpaleye :: Query a -> Opaleye.Query a
toOpaleye (Query q) = q


mapOpaleye :: (Opaleye.Query a -> Opaleye.Query b) -> Query a -> Query b
mapOpaleye f = liftOpaleye . f . toOpaleye


instance Monad Query where
  return = pure
  Query ( Opaleye.QueryArr f ) >>= g = Query $ Opaleye.QueryArr \input ->
    case f input of
      ( a, primQuery, tag ) ->
        case g a of
          Query ( Opaleye.QueryArr h ) ->
            h ( (), primQuery, tag )


-- | Run a @SELECT@ query, returning all rows.
select :: forall row haskell m. (New.Serializable row haskell, MonadIO m) => Connection -> Query row -> m [haskell]
select conn query = liftIO case selectQuery query of
  Nothing -> return []
  Just neQuery ->
    Hasql.run session conn >>= either throwIO return
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement q params (Hasql.rowList (New.parse @row)) prepare
      q = encodeUtf8 (pack neQuery)
      params = Hasql.noParams
      prepare = False


---- | Run an @INSERT@ statement
----
---- >>> :{
---- insert c Insert
----   { into = authorSchema
----   , rows = [ lit Author{ authorName = "Gabriel Gonzales", authorId = AuthorId 4, authorUrl = Just "https://haskellforall.com" } ]
----   , onConflict = Abort
----   , returning = NumberOfRowsAffected
----   }
---- :}
---- 1
--insert :: MonadIO m => Connection -> Insert result -> m result
--insert conn Insert{ into, rows, onConflict, returning } = liftIO
--  case (rows, returning) of
--    ([], NumberOfRowsAffected) ->
--      return 0

--    ([], Projection _) ->
--      return []

--    (x:xs, NumberOfRowsAffected) -> Hasql.run session conn >>= either throwIO return where
--      session = Hasql.statement () statement where
--        statement = Hasql.Statement q Hasql.noParams Hasql.rowsAffected False where
--          q = encodeUtf8 $ pack $ Opaleye.arrangeInsertManySql table neRows maybeOnConflict where
--            table = ddlTable into (writer into)
--            neRows = x :| xs

--    (x:xs, Projection p) -> Hasql.run session conn >>= either throwIO return where
--      session = Hasql.statement () statement where
--        statement = Hasql.Statement q Hasql.noParams (mkDecoder p) False where
--          q = encodeUtf8 $ pack $ Opaleye.arrangeInsertManyReturningSql unpackspec table neRows f maybeOnConflict where
--            f  = p . mapTable (column . columnName)
--            table = ddlTable into (writer into)
--            neRows = x :| xs

--          mkDecoder :: forall row projection a. Serializable projection a => (row -> projection) -> Hasql.Result [a]
--          mkDecoder _ = Hasql.rowList (hasqlRowDecoder @projection)
--  where
--    maybeOnConflict =
--      case onConflict of
--        DoNothing -> Just Opaleye.DoNothing
--        Abort     -> Nothing


--writer
--  :: forall value schema
--   . Selects schema value
--  => TableSchema schema -> Opaleye.Writer value schema
--writer into_ =
--  let
--    go
--      :: forall f list
--       . ( Functor list, Applicative f )
--      => ( ( list Opaleye.PrimExpr, String ) -> f () )
--      -> list value
--      -> f ()
--    go f xs =
--      void $
--        htraverse @(Columns schema) @_ @Expr getCompose $
--          htabulate @(Columns schema) @(Compose f Expr) \i ->
--            case hfield (toColumns (tableColumns into_)) i of
--              ColumnSchema{ columnName } ->
--                Compose $
--                  column columnName <$
--                  f ( toPrimExpr . flip hfield i . toColumns <$> xs
--                    , columnName
--                    )

--  in
--  Opaleye.Writer ( Opaleye.PackMap go )


--ddlTable :: TableSchema schema -> Opaleye.Writer value schema -> Opaleye.Table value schema
--ddlTable schema writer_ =
--  toOpaleyeTable schema writer_ $ Opaleye.View (tableColumns schema)


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: Type -> Type where
  Insert
    :: { into :: TableSchema schema
         -- ^ Which table to insert into.
       , rows :: [value]
         -- ^ The rows to insert.
       , onConflict :: OnConflict
         -- ^ What to do if the inserted rows conflict with data already in the
         -- table.
       , returning :: Returning schema result
         -- ^ What information to return on completion.
       }
    -> Insert result


-- | @Returning@ describes what information to return when an @INSERT@
-- statement completes.
data Returning schema a where
  -- | Just return the number of rows inserted.
  NumberOfRowsAffected :: Returning schema Int64

  -- | Return a projection of the rows inserted. This can be useful if your
  -- insert statement increments sequences by using default values.
  --
  -- >>> :t insert Insert{ returning = Projection fooId }
  -- IO [ FooId ]
  Projection
    :: New.Serializable projection a
    => (row -> projection)
    -> Returning schema [a]


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
data OnConflict
  = Abort     -- ^ @ON CONFLICT ABORT@
  | DoNothing -- ^ @ON CONFLICT DO NOTHING@


selectQuery :: (New.Context a ~ New.DB, New.Table a) => Query a -> Maybe String
selectQuery (Query opaleye) = showSqlForPostgresExplicit
  where
    showSqlForPostgresExplicit =
      case Opaleye.runQueryArrUnpack New.unpackspec opaleye of
        (x, y, z) -> Opaleye.formatAndShowSQL True (x , Opaleye.optimize y, z)


-- | Run a @DELETE@ statement.
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
--
-- >>> :{
-- delete c Delete
--   { from = projectSchema
--   , deleteWhere = \p -> projectName p ==. lit "rel8"
--   , returning = Projection projectName
--   }
-- :}
-- ["rel8"]
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
-- delete :: MonadIO m => Connection -> Delete from returning -> m returning
-- delete conn Delete{ from = deleteFrom, deleteWhere, returning } = liftIO
--   case returning of
--     NumberOfRowsAffected -> Hasql.run session conn >>= either throwIO return where
--       session = Hasql.statement () statement where
--         statement = Hasql.Statement q Hasql.noParams Hasql.rowsAffected False where
--           q = encodeUtf8 $ pack $ Opaleye.arrangeDeleteSql table f where
--             f = Opaleye.Column . toPrimExpr . deleteWhere . mapTable (column . columnName)
--             table = ddlTable deleteFrom (writer deleteFrom)

    -- Projection p -> Hasql.run session conn >>= either throwIO return where
    --   session = Hasql.statement () statement where
    --     statement = Hasql.Statement q Hasql.noParams (mkDecoder p) False where
    --       q = encodeUtf8 $ pack $ Opaleye.arrangeDeleteReturningSql unpackspec table f g where
    --         f = Opaleye.Column . toPrimExpr . deleteWhere . mapTable (column . columnName)
    --         table = ddlTable deleteFrom (writer deleteFrom)
    --         g = p . mapTable (column . columnName)

    --       mkDecoder :: forall row projection a. Serializable projection a => (row -> projection) -> Hasql.Result [a]
    --       mkDecoder _ = Hasql.rowList (hasqlRowDecoder @projection)


-- | The constituent parts of a @DELETE@ statement.
data Delete from return where
  Delete
    :: { from :: TableSchema from
         -- ^ Which table to delete from.
       , deleteWhere :: row -> Expr Bool
         -- ^ Which rows should be selected for deletion.
       , returning :: Returning from return
         -- ^ What to return from the @DELETE@ statement.
       }
    -> Delete from return


---- | Run an @UPDATE@ statement.
----
---- >>> mapM_ print =<< select c (each projectSchema)
---- Project {projectAuthorId = 1, projectName = "rel8"}
---- Project {projectAuthorId = 2, projectName = "aeson"}
---- Project {projectAuthorId = 2, projectName = "text"}
----
---- >>> :{
---- update c Update
----   { target = projectSchema
----   , set = \p -> p { projectName = "Rel8!" }
----   , updateWhere = \p -> projectName p ==. lit "rel8"
----   , returning = NumberOfRowsAffected
----   }
---- :}
---- 1
----
---- >>> mapM_ print =<< select c (each projectSchema)
---- Project {projectAuthorId = 2, projectName = "aeson"}
---- Project {projectAuthorId = 2, projectName = "text"}
---- Project {projectAuthorId = 1, projectName = "Rel8!"}
--update :: MonadIO m => Connection -> Update target returning -> m returning
--update conn Update{ target, set, updateWhere, returning } = liftIO
--  case returning of
--    NumberOfRowsAffected -> Hasql.run session conn >>= either throwIO return where
--      session = Hasql.statement () statement where
--        statement = Hasql.Statement q Hasql.noParams Hasql.rowsAffected False where
--          q = encodeUtf8 $ pack $ Opaleye.arrangeUpdateSql table g f where
--            f = Opaleye.Column . toPrimExpr . updateWhere . mapTable (column . columnName)
--            g = set . mapTable (column . columnName)

--    Projection p -> Hasql.run session conn >>= either throwIO return where
--      session = Hasql.statement () statement where
--        statement = Hasql.Statement q Hasql.noParams (mkDecoder p) False where
--          q = encodeUtf8 $ pack $ Opaleye.arrangeUpdateReturningSql unpackspec table g f h where
--            f = Opaleye.Column . toPrimExpr . updateWhere . mapTable (column . columnName)
--            g = set . mapTable (column . columnName)
--            h = p . mapTable (column . columnName)

--          mkDecoder :: forall row projection a. Serializable projection a => (row -> projection) -> Hasql.Result [a]
--          mkDecoder _ = Hasql.rowList (hasqlRowDecoder @projection)
--  where
--    table = ddlTable target (writer target)


-- | The constituent parts of an @UPDATE@ statement.
data Update target returning where
  Update
    :: { target :: TableSchema target
         -- ^ Which table to update.
       , set :: row -> row
         -- ^ How to update each selected row.
       , updateWhere :: row -> Expr Bool
         -- ^ Which rows to select for update.
       , returning :: Returning target returning
         -- ^ What to return from the @UPDATE@ statement.
       }
    -> Update target returning


-- | Checks if a query returns at least one row.
--
-- >>> :{
-- mapM_ print =<< select c do
--   author <- each authorSchema
--   hasProjects <- exists do
--     project <- each projectSchema
--     where_ $ authorId author ==. projectAuthorId project
--   return (authorName author, hasProjects)
-- :}
-- ("Ollie",True)
-- ("Bryan O'Sullivan",True)
-- ("Emily Pillmore",False)
exists :: Query a -> Query (Expr Bool)
exists = fmap (Expr . New.columnToExpr) . mapOpaleye Opaleye.exists


-- | Select each row from a table definition. This is equivalent to @FROM
-- table@.
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
each :: forall schema row inserts. (New.Context row ~ New.DB, New.Table row, New.Recontextualize New.Name New.DB schema row, New.Recontextualize New.Name New.Insert schema inserts) => TableSchema schema -> Query row
each = liftOpaleye . Opaleye.selectTableExplicit New.unpackspec . f
  where
    f :: TableSchema schema -> Opaleye.Table inserts row
    f schema = toOpaleyeTable schema $ New.tableFields $ tableColumns schema


-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, a null 'MaybeTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
optional :: Query a -> Query (New.MaybeTable a)
optional = mapOpaleye $ Opaleye.laterally (Opaleye.QueryArr . go)
  where
    go query (i, left, tag) = (New.MaybeTable t' a, join, Opaleye.next tag')
      where
        (New.MaybeTable t a, right, tag') = Opaleye.runSimpleQueryArr (pure <$> query) (i, tag)
        (t', bindings) = Opaleye.run $ Opaleye.runUnpackspec New.unpackspec (Opaleye.extractAttr "maybe" tag') t
        join = Opaleye.Join Opaleye.LeftJoin (New.toPrimExpr $ New.lit True) [] bindings left right


-- | Combine the results of two queries of the same type, collapsing
-- duplicates.  @union a b@ is the same as the SQL statement @x UNION b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `union` values [lit True]
-- [False,True]
union :: (New.Table a, New.Context a ~ New.DB) => Query a -> Query a -> Query a
union l r = liftOpaleye $ Opaleye.unionExplicit New.binaryspec (toOpaleye l) (toOpaleye r)


-- | Combine the results of two queries of the same type, retaining duplicates.
-- @unionAll a b@ is the same as the SQL statement @x UNION ALL b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `unionAll` values [lit True]
-- [True,True,False,True]
unionAll :: (New.Table a, New.Context a ~ New.DB) => Query a -> Query a -> Query a
unionAll l r = liftOpaleye $ Opaleye.unionAllExplicit New.binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the intersection of two queries, collapsing duplicates.  @intersect a
-- b@ is the same as the SQL statement @x INTERSECT b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `intersect` values [lit True]
-- [True]
intersect :: (New.Table a, New.Context a ~ New.DB) => Query a -> Query a -> Query a
intersect l r = liftOpaleye $ Opaleye.intersectExplicit New.binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the intersection of two queries, retaining duplicates.  @intersectAll
-- a b@ is the same as the SQL statement @x INTERSECT ALL b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `intersectAll` values [lit True, lit True]
-- [True,True]
intersectAll :: (New.Table a, New.Context a ~ New.DB) => Query a -> Query a -> Query a
intersectAll l r = liftOpaleye $ Opaleye.intersectAllExplicit New.binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the difference of two queries, collapsing duplicates @except a b@ is
-- the same as the SQL statement @x INTERSECT b@.
--
-- >>> select c $ values [lit True, lit False, lit False] `except` values [lit True]
-- [False]
except :: (New.Table a, New.Context a ~ New.DB) => Query a -> Query a -> Query a
except l r = liftOpaleye $ Opaleye.exceptExplicit New.binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the difference of two queries, retaining duplicates.  @exceptAll a b@
-- is the same as the SQL statement @x EXCEPT ALL b@.
--
-- >>> select c $ values [lit True, lit False, lit False] `exceptAll` values [lit True]
-- [False,False]
exceptAll :: (New.Table a, New.Context a ~ New.DB) => Query a -> Query a -> Query a
exceptAll l r = liftOpaleye $ Opaleye.exceptAllExplicit New.binaryspec (toOpaleye l) (toOpaleye r)


-- | Select all distinct rows from a query, removing duplicates.  @distinct q@
-- is equivalent to the SQL statement @SELECT DISTINCT q@.
--
-- >>> select c $ distinct $ values [ lit True, lit True, lit False ]
-- [False,True]
distinct :: (New.Table a, New.Context a ~ New.DB) => Query a -> Query a
distinct = mapOpaleye (Opaleye.distinctExplicit New.distinctspec)


distinctOn :: (New.Table b, New.Context b ~ New.DB) => (a -> b) -> Query a -> Query a
distinctOn proj = 
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOn New.unpackspec proj . Opaleye.runSimpleQueryArr q))


distinctOnBy :: (New.Context b ~ New.DB, New.Table b) => (a -> b) -> Order a -> Query a -> Query a
distinctOnBy proj (Order order) = 
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOnBy New.unpackspec proj order . Opaleye.runSimpleQueryArr q))


-- | @limit n@ select at most @n@ rows from a query.  @limit n@ is equivalent
-- to the SQL @LIMIT n@.
--
-- >>> select c $ limit 3 $ values [ lit x | x <- [ 1..5 :: Int32 ] ]
-- [1,2,3]
limit :: Natural -> Query a -> Query a
limit n = mapOpaleye $ Opaleye.limit (fromIntegral n)


-- | @offset n@ drops the first @n@ rows from a query. @offset n@ is equivalent
-- to the SQL @OFFSET n@.
--
-- >>> select c $ offset 3 $ values [ lit x | x <- [ 1..5 :: Int32 ] ]
-- [4,5]
offset :: Natural -> Query a -> Query a
offset n = mapOpaleye $ Opaleye.offset (fromIntegral n)


-- | Drop any rows that don't match a predicate.  @where_ expr@ is equivalent
-- to the SQL @WHERE expr@.
--
-- >>> :{
-- select c $ do
--   x <- values [ lit x | x <- [ 1..5 :: Int32 ] ]
--   where_ $ x >. lit 2
--   return x
-- :}
-- [3,4,5]
where_ :: Expr Bool -> Query ()
where_ (Expr x) =
  liftOpaleye $ Opaleye.QueryArr \((), left, t) ->
    ((), Opaleye.restrict (New.toPrimExpr x) left, t)


-- | Produce the empty query if the given query returns no rows. @whereExists@
-- is equivalent to @WHERE EXISTS@ in SQL.
--
-- >>> :{
-- select c do
--   author <- each authorSchema
--   whereExists do
--     project <- each projectSchema
--     where_ $ projectAuthorId project ==. authorId author
--   return $ authorName author
-- :}
-- ["Ollie","Bryan O'Sullivan"]
whereExists :: Query a -> Query ()
whereExists = mapOpaleye Opaleye.restrictExists


-- | Produce the empty query if the given query returns rows. @whereNotExists@
-- is equivalent to @WHERE NOT EXISTS@ in SQL.
--
-- >>> :{
-- select c do
--   author <- each authorSchema
--   whereNotExists do
--     project <- each projectSchema
--     where_ $ projectAuthorId project ==. authorId author
--   return $ authorName author
-- :}
-- ["Emily Pillmore"]
whereNotExists :: Query a -> Query ()
whereNotExists = mapOpaleye Opaleye.restrictNotExists


-- | Filter out 'MaybeTable's, returning only the tables that are not-null.
--
-- This operation can be used to "undo" the effect of 'optional', which
-- operationally is like turning a @LEFT JOIN@ back into a full @JOIN@.  You
-- can think of this as analogous to 'Data.Maybe.catMaybes'.
--
-- To see this in action, first consider the following 'optional' query:
--
-- >>> :{
-- select c $ do
--   author <- each authorSchema
--   maybeRel8 <- optional $ 
--     each projectSchema 
--       >>= filter (\p -> projectAuthorId p ==. authorId author)
--       >>= filter (\p -> projectName p ==. "rel8")
--   return (authorName author, projectName <$> maybeRel8)
-- :}
-- [("Ollie",Just "rel8"),("Bryan O'Sullivan",Nothing),("Emily Pillmore",Nothing)]
--
-- Here @optional@ is acting as a @LEFT JOIN@. We can turn this into a proper
-- join by using @catMaybeTable@ to filter out rows where the join failed:
--
-- >>> :{
-- select c $ do
--   author <- each authorSchema
--   maybeRel8 <- optional $ 
--     each projectSchema 
--       >>= filter (\p -> projectAuthorId p ==. authorId author)
--       >>= filter (\p -> projectName p ==. "rel8")
--   rel8 <- catMaybeTable maybeRel8
--   return (authorName author, projectName rel8)
-- :}
-- [("Ollie","rel8")]
catMaybeTable :: New.MaybeTable a -> Query a
catMaybeTable New.MaybeTable{ tag, just } = do
  where_ $ not_ $ Expr $ New.isNull tag
  return just


-- | Construct a query that returns the given input list of rows. This is like
-- folding a list of 'return' statements under 'union', but uses the SQL
-- @VALUES@ expression for efficiency.
--
-- Typically @values@ will be used with 'lit':
--
-- >>> mapM_ Data.Text.IO.putStrLn =<< select c (values [ lit "Hello", lit "World!" ])
-- Hello
-- World!
values :: (Foldable f, New.Context a ~ New.DB, New.Table a) => f a -> Query a
values = liftOpaleye . Opaleye.valuesExplicit New.valuesspec . toList


-- | @filter f x@ will be a zero-row query when @f x@ is @False@, and will
-- return @x@ unchanged when @f x@ is @True@. This is similar to
-- 'Control.Monad.guard', but as the predicate is separate from the argument,
-- it is easy to use in a pipeline of 'Query' transformations.
--
-- >>> select c $ values [ lit x | x <- [ 1..5 :: Int32 ] ] >>= filter (>. 3)
-- [4,5]
filter :: (a -> Expr Bool) -> a -> Query a
filter f a = do
  where_ $ f a
  return a


toOpaleyeTable
  :: TableSchema schema
  -> Opaleye.TableFields write view
  -> Opaleye.Table write view
toOpaleyeTable TableSchema{ tableName, tableSchema } tableFields =
  maybe withoutSchema withSchema tableSchema
  where
    withoutSchema = Opaleye.Table tableName tableFields
    withSchema s = Opaleye.TableWithSchema s tableName tableFields


-- | Convert a query to a 'String' containing the query as a @SELECT@
-- statement.
showQuery :: (New.Context a ~ New.DB, New.Table a) => Query a -> String
showQuery = fold . selectQuery


-- | An @Aggregate a@ describes how to aggregate @Table@s of type @a@. You can
-- unpack an @Aggregate@ back to @a@ by running it with 'aggregate'. As
-- @Aggregate@ is an 'Applicative' functor, you can combine @Aggregate@s using
-- the normal @Applicative@ combinators, or by working in @do@ notation with
-- @ApplicativeDo@.
newtype Aggregate a = Aggregate a


instance Functor Aggregate where
  fmap f (Aggregate a) = Aggregate $ f a


instance Applicative Aggregate where
  pure = Aggregate
  Aggregate f <*> Aggregate a = Aggregate $ f a


-- | Aggregate a value by grouping by it. @groupBy@ is just a synonym for
-- 'pure', but sometimes being explicit can help the readability of your code.
groupBy :: a -> Aggregate a
groupBy = pure


-- | Aggregate rows into a single row containing an array of all aggregated
-- rows. This can be used to associate multiple rows with a single row, without
-- changing the over cardinality of the query. This allows you to essentially
-- return a tree-like structure from queries.
--
-- For example, if we have a table of orders and each orders contains multiple
-- items, we could aggregate the table of orders, pairing each order with its
-- items:
--
-- @
-- ordersWithItems :: Query (Order Expr, ListTable (Item Expr))
-- ordersWithItems = do
--   order <- each orderSchema
--   items <- aggregate $ listAgg <$> itemsFromOrder order
--   return (order, items)
-- @
listAgg :: (New.Table exprs, New.Context exprs ~ New.DB) => exprs -> Aggregate (New.ListTable exprs)
listAgg = Aggregate . New.ListTable . New.hvectorize (\_ (Identity (New.DB a)) -> go a) . pure . New.toColumns
  where
    go a = New.DB $ New.fromPrimExpr $ Opaleye.FunExpr "row" [Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrArr (New.toPrimExpr a) []]


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: (New.Table exprs, New.Context exprs ~ New.DB) => exprs -> Aggregate (New.NonEmptyTable exprs)
nonEmptyAgg = Aggregate . New.NonEmptyTable . New.hvectorize (\_ (Identity (New.DB a)) -> go a) . pure . New.toColumns
  where
    go a = New.DB $ New.fromPrimExpr $ Opaleye.FunExpr "row" [Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrArr (New.toPrimExpr a) []]


-- | The class of 'DBType's that support the @max@ aggregation function.
--
-- If you have a custom type that you know supports @max@, you can use
-- @DeriveAnyClass@ to derive a default implementation that calls @max@.
class DBMax a where
  -- | Produce an aggregation for @Expr a@ using the @max@ function.
  max :: Expr a -> Aggregate (Expr a)
  max (Expr a) = Aggregate $ Expr $ New.fromPrimExpr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrMax (New.toPrimExpr a) []


instance DBMax Int64
instance DBMax Double
instance DBMax Int16
instance DBMax Int32
instance DBMax Scientific
instance DBMax Float
instance DBMax Text
instance DBMax UTCTime
instance DBMax a => DBMax (Maybe a)


-- | The class of 'DBType's that support the @min@ aggregation function.
--
-- If you have a custom type that you know supports @min@, you can use
-- @DeriveAnyClass@ to derive a default implementation that calls @min@.
class DBMin a where
  -- | Produce an aggregation for @Expr a@ using the @max@ function.
  min :: Expr a -> Aggregate (Expr a)
  min (Expr a) = Aggregate $ Expr $ New.fromPrimExpr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrMin (New.toPrimExpr a) []


instance DBMin Int64
instance DBMin Double
instance DBMin Int16
instance DBMin Int32
instance DBMin Scientific
instance DBMin Float
instance DBMin Text
instance DBMin UTCTime
instance DBMax a => DBMin (Maybe a) -- TODO: Do we want this?


-- | The class of data types that can be aggregated under the @sum@ operation.
-- This type class contains two parameters, as @sum@ can be a type-changing
-- operation in PostgreSQL.
class New.DBType a => DBSum a res | a -> res where
  -- | Corresponds to @sum@.
  sum :: Expr a -> Aggregate (Expr res)
  sum (Expr a) = Aggregate $ Expr $ New.fromPrimExpr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrSum (New.toPrimExpr a) []


instance DBSum Int64 Scientific
instance DBSum Double Double
instance DBSum Int32 Int64
instance DBSum Scientific Scientific
instance DBSum Float Float
instance DBSum Int16 Int64


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: forall a. (New.Table a, New.Context a ~ New.DB) => Query (Aggregate a) -> Query a
aggregate = mapOpaleye $ Opaleye.aggregate aggregator
  where
    aggregator :: Opaleye.Aggregator (Aggregate a) a
    aggregator = Opaleye.Aggregator $ Opaleye.PackMap \f (Aggregate x) ->
      New.fromColumns <$> New.htraverse (g f) (New.toColumns x)

    g :: forall f spec. Applicative f => ((Maybe (Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct), Opaleye.PrimExpr) -> f Opaleye.PrimExpr) -> New.DB spec -> f (New.DB spec)
    g f (New.DB expr) = New.DB . New.fromPrimExpr <$> traverseAggrExpr f (New.toPrimExpr expr)


traverseAggrExpr :: Applicative f 
  => ((Maybe (Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct), Opaleye.PrimExpr) -> f Opaleye.PrimExpr) 
  -> Opaleye.PrimExpr 
  -> f Opaleye.PrimExpr
traverseAggrExpr f = \case
  Opaleye.AggrExpr a b c d ->
    f (Just (b, d, a), c)

  Opaleye.AttrExpr symbol -> 
    -- TODO Test me
    f (Nothing, Opaleye.AttrExpr symbol)

  Opaleye.BaseTableAttrExpr attribute ->
    -- TODO Test me
    f (Nothing, Opaleye.BaseTableAttrExpr attribute)

  Opaleye.CompositeExpr primExpr x ->
    Opaleye.CompositeExpr <$> traverseAggrExpr f primExpr <*> pure x

  Opaleye.BinExpr x primExpr1 primExpr2 ->
    Opaleye.BinExpr x <$> traverseAggrExpr f primExpr1 <*> traverseAggrExpr f primExpr2

  Opaleye.UnExpr x primExpr ->
    Opaleye.UnExpr x <$> traverseAggrExpr f primExpr

  Opaleye.CaseExpr cases def ->
    Opaleye.CaseExpr <$> traverse (traverseBoth (traverseAggrExpr f)) cases <*> traverseAggrExpr f def
    where traverseBoth g (x, y) = (,) <$> g x <*> g y

  Opaleye.ListExpr elems ->
    Opaleye.ListExpr <$> traverse (traverseAggrExpr f) elems

  Opaleye.ParamExpr p primExpr ->
    Opaleye.ParamExpr p <$> traverseAggrExpr f primExpr

  Opaleye.FunExpr name params ->
    Opaleye.FunExpr name <$> traverse (traverseAggrExpr f) params

  Opaleye.CastExpr t primExpr ->
    Opaleye.CastExpr t <$> traverseAggrExpr f primExpr

  Opaleye.ArrayExpr elems ->
    Opaleye.ArrayExpr <$> traverse (traverseAggrExpr f) elems

  Opaleye.RangeExpr a b c ->
    Opaleye.RangeExpr a <$> traverseBoundExpr (traverseAggrExpr f) b <*> traverseBoundExpr (traverseAggrExpr f) c
    where
      traverseBoundExpr g = \case
        Opaleye.Inclusive primExpr -> Opaleye.Inclusive <$> g primExpr
        Opaleye.Exclusive primExpr -> Opaleye.Exclusive <$> g primExpr
        other                      -> pure other

  Opaleye.ArrayIndex x i ->
    Opaleye.ArrayIndex <$> traverseAggrExpr f x <*> traverseAggrExpr f i

  other ->
    -- All other constructors that don't contain any PrimExpr's.
    pure other


-- | Aggregate a 'Query' into a 'ListTable'. If the supplied query returns 0
-- rows, this function will produce a 'Query' that returns one row containing
-- the empty @ListTable@. If the supplied @Query@ does return rows, @many@ will
-- return exactly one row, with a @ListTable@ collecting all returned rows.
-- 
-- @many@ is analogous to 'Control.Applicative.many' from
-- @Control.Applicative@.
many :: (New.Context exprs ~ New.DB, New.Table exprs) => Query exprs -> Query (New.ListTable exprs)
many = fmap (maybeTable mempty id) . optional . aggregate . fmap listAgg


-- | Aggregate a 'Query' into a 'NonEmptyTable'. If the supplied query returns
-- 0 rows, this function will produce a 'Query' that is empty - that is, will
-- produce zero @NonEmptyTable@s. If the supplied @Query@ does return rows,
-- @some@ will return exactly one row, with a @NonEmptyTable@ collecting all
-- returned rows.
--
-- @some@ is analogous to 'Control.Applicative.some' from
-- @Control.Applicative@.
some :: (New.Context exprs ~ New.DB, New.Table exprs) => Query exprs -> Query (New.NonEmptyTable exprs)
some = aggregate . fmap nonEmptyAgg


-- | An ordering expression for @a@. Primitive orderings are defined with 'asc'
-- and 'desc', and you can combine @Order@ via its various instances.
--
-- A common pattern is to use '<>' to combine multiple orderings in sequence,
-- and '>$<' (from 'Contravariant') to select individual columns. For example,
-- to sort a @Query@ on two columns, we could do:
--
-- >>> import Data.Functor.Contravariant ((>$<))
-- >>> :{
-- select c $ orderBy (mconcat [fst >$< asc, snd >$< desc]) $ do
--   x <- values [ lit x | x <- [1..3 :: Int32 ] ]
--   y <- values [ lit x | x <- [1..3 :: Int32 ] ]
--   return (x, y)
-- :}
-- [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
newtype Order a = Order (Opaleye.Order a)
  deriving newtype (Contravariant, Divisible, Decidable, Semigroup, Monoid)


-- | Sort a column in ascending order.
--
-- >>> select c $ orderBy asc $ values [ lit x | x <- [1..5 :: Int32] ]
-- [1,2,3,4,5]
asc :: New.Table (Expr a) => Order (Expr a)
asc = Order $ Opaleye.Order (getConst . New.htraverse f . New.toColumns)
  where
    f :: New.DB spec -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (g spec)
    f (New.DB expr) = Const [(orderOp, New.toPrimExpr expr)]

    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpAsc
      , orderNulls = Opaleye.NullsLast
      }


-- | Sort a column in descending order.
--
-- >>> select c $ orderBy desc $ values [ lit x | x <- [1..5 :: Int32] ]
-- [5,4,3,2,1]
desc :: New.Table (Expr a) => Order (Expr a)
desc = Order $ Opaleye.Order (getConst . New.htraverse f . New.toColumns)
  where
    f :: New.DB spec -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (g spec)
    f (New.DB expr) = Const [(orderOp, New.toPrimExpr expr)]

    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpDesc
      , orderNulls = Opaleye.NullsFirst
      }


-- | Transform an ordering so that @null@ values appear first. This corresponds
-- to @NULLS FIRST@ in SQL.
--
-- >>> select c $ orderBy (nullsFirst desc) $ values $ [ nullExpr, nullExpr ] <> [ lit (Just x) | x <- [1..5 :: Int32] ]
-- [Nothing,Nothing,Just 5,Just 4,Just 3,Just 2,Just 1]
nullsFirst :: Order (Expr a) -> Order (NullExpr a)
nullsFirst (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f . retype
  where
    retype :: forall a. NullExpr a -> Expr a
    retype (NullExpr a) = Expr $ New.unsafeUnnullify a

    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsFirst }


-- | Transform an ordering so that @null@ values appear first. This corresponds
-- to @NULLS LAST@ in SQL.
--
-- >>> select c $ orderBy (nullsLast desc) $ values $ [ nullExpr, nullExpr ] <> [ lit (Just x) | x <- [1..5 :: Int32] ]
-- [Just 5,Just 4,Just 3,Just 2,Just 1,Nothing,Nothing]
nullsLast :: Order (Expr a) -> Order (NullExpr a)
nullsLast (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f . retype
  where
    retype :: forall a. NullExpr a -> Expr a
    retype (NullExpr a) = Expr $ New.unsafeUnnullify a

    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsLast }



-- | Order the rows returned by a 'Query' according to a particular 'Order'.
--
-- For an example of using this, see the documentation for 'Order'.
orderBy :: Order a -> Query a -> Query a
orderBy (Order o) = liftOpaleye . Opaleye.laterally (Opaleye.orderBy o) . toOpaleye


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
count :: Expr a -> Aggregate (Expr Int64)
count (Expr a) = Aggregate $ Expr $ New.fromPrimExpr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrCount (New.toPrimExpr a) []


-- | Corresponds to @COUNT(*)@.
countStar :: Aggregate (Expr Int64)
countStar = count (0 :: Expr Int64)


-- | Count the number of distinct occurances of a single column. Corresponds to
-- @COUNT(DISTINCT a)@
countDistinct :: Expr a -> Aggregate (Expr Int64)
countDistinct (Expr a) = Aggregate $ Expr $ New.fromPrimExpr $ Opaleye.AggrExpr Opaleye.AggrDistinct Opaleye.AggrCount (New.toPrimExpr a) []


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
countRows :: Query a -> Query (Expr Int64)
countRows = fmap (Expr . New.columnToExpr) . mapOpaleye Opaleye.countRows


-- | Corresponds to @bool_or@.
boolOr :: Expr Bool -> Aggregate (Expr Bool)
boolOr (Expr a) = Aggregate $ Expr $ New.fromPrimExpr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrBoolOr (New.toPrimExpr a) []


-- | Corresponds to @bool_and@.
boolAnd :: Expr Bool -> Aggregate (Expr Bool)
boolAnd (Expr a) = Aggregate $ Expr $ New.fromPrimExpr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrBoolAnd (New.toPrimExpr a) []


class Eqy a where
  (==.) :: a -> a -> Expr Bool


instance New.DBEq a => Eqy (Expr a) where
  Expr a ==. Expr b = Expr $ a New.==. b


instance New.DBEq a => Eqy (NullExpr a) where
  NullExpr a ==. NullExpr b = Expr $ a New.==. b


type family HMaybe (context :: Type -> Type) (a :: Type) :: Type where
 
--type family HNonEmpty (context :: Type -> Type) (a :: Type) :: Type where
--  HNonEmpty Identity a = NonEmpty a
--  HNonEmpty Expr a     = NonEmptyTable a
--  HNonEmpty f a        = HComposeTable NonEmpty (Columns a) (Context f)


