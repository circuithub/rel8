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
    DBType(..)

    -- *** Deriving-via helpers
    -- **** @JSONEncoded@
  , JSONEncoded(..)

    -- **** @ReadShow@
  , ReadShow(..)

    -- *** @DatabaseType@
  , DatabaseType(..)
  , mapDatabaseType
  , parseDatabaseType

    -- ** @DBEq@
  , DBEq(..)

    -- * Tables and higher-kinded tables
  , Table(..)
  , HTable
  , HigherKindedTable
  , Congruent
  , KContext
  , Context

    -- ** Table schemas
  , Column
  , TableSchema(..)
  , ColumnSchema

    -- * Expressions
  , Expr
  , unsafeCastExpr
  , unsafeCoerceExpr
  , unsafeLiteral
  , binaryOperator

    -- ** @null@
  , nullExpr
  , null_
  , isNull
  , liftNull
  , mapNull
  , liftOpNull
  , catMaybe

    -- ** Boolean operations
  , (&&.)
  , and_
  , (||.)
  , or_
  , not_
  , in_
  , ifThenElse_
  , EqTable(..)

    -- ** Ordering
  , DBOrd(..)

    -- ** Functions
  , Function
  , function
  , nullaryFunction

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
  , distinct

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
  , MaybeTable
  , HMaybe
  , optional
  , maybeTable
  , noTable
  , catMaybeTable
  , exists

    -- ** Aggregation
  , Aggregate
  , aggregate
  , listAgg
  , nonEmptyAgg
  , groupBy
  , DBMax( max )

    -- *** List aggregation
  , ListTable
  , HList
  , many
  , NonEmptyTable
  , HNonEmpty
  , some

    -- ** Ordering
  , orderBy
  , Order
  , asc
  , desc
  , nullsFirst
  , nullsLast

    -- * IO
  , Serializable(..)
  , ExprFor

    -- * Running statements
    -- ** @SELECT@
  , select

    -- ** @INSERT@
  , Insert(..)
  , OnConflict(..)
  , insert

    -- ** @DELETE@
  , Delete(..)
  , delete

    -- ** @UPDATE@
  , update
  , Update(..)

    -- ** @.. RETURNING@
  , Returning(..)
  ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, Value( Number, Bool, Null, Array, String ), decode, decodeStrict, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- attoparsec
import Data.Attoparsec.ByteString.Char8 hiding ( Result )

-- base
import Control.Applicative ( ZipList(..), liftA2, liftA3, Alternative ((<|>)) )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Bifunctor ( first )
import Data.Foldable ( fold, foldl', toList )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Contravariant ( Contravariant )
import Data.Functor.Identity ( Identity( Identity, runIdentity ) )
import Data.Int ( Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Data.Monoid ( Any( Any ), getAny )
import Data.Proxy ( Proxy( Proxy ) )
import Data.String ( IsString(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( (:*:)(..), Generic, K1(..), M1(..), Rep, from, to )
import Numeric.Natural ( Natural )
import Prelude hiding ( filter, max )
import Text.Read ( readEither )

-- bytestring
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy ( fromStrict )
import qualified Data.ByteString.Lazy 

-- contravariant
import Data.Functor.Contravariant.Divisible ( Decidable, Divisible )

-- opaleye
import qualified Opaleye ( Delete(..), Insert(..), OnConflict(..), PGInt8, Update(..), runDelete_, runInsert_, runUpdate_, valuesExplicit )
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
import qualified Opaleye.Internal.PrimQuery as Opaleye hiding ( BinOp, aggregate, limit )
import qualified Opaleye.Internal.Print as Opaleye ( formatAndShowSQL )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Lateral as Opaleye
import qualified Opaleye.Operators as Opaleye hiding ( restrict )
import qualified Opaleye.Order as Opaleye
import Opaleye.PGTypes
  ( IsSqlType(..)
  , pgBool
  , pgInt4
  , pgInt8
  , pgNumeric
  , pgStrictText
  , pgValueJSON, pgUTCTime, pgDay, pgLocalTime, pgTimeOfDay, pgZonedTime, pgUUID, pgStrictByteString, pgDouble
  )
import qualified Opaleye.Table as Opaleye

-- postgresql-simple
import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( Connection )
import Database.PostgreSQL.Simple.FromField
  ( Conversion
  , ResultError( Incompatible )
  , returnError
  )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import qualified Database.PostgreSQL.Simple.FromRow as Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal ( RowParser( RP ) )

-- rel8
import qualified Rel8.Optimize

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )

-- transformers
import Control.Monad.Trans.Class ( lift )
import Data.Time (UTCTime, Day, LocalTime, ZonedTime, TimeOfDay)
import Database.PostgreSQL.Simple.Time (parseUTCTime, parseDay, parseLocalTime, parseTimeOfDay, parseZonedTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Text.Lazy
import Database.PQ (unescapeBytea)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.List.NonEmpty as NonEmpty


-- $setup
--
-- In this section, we'll take a look at how Rel8 can be used to work with a
-- simple schema for Haskell packages. We'll take a look at idiomatic usage of
-- Rel8, defining custom tables and types, and writing some simple queries with
-- this schema. 
--
-- Before we look at any Haskell code, let's take a look at the schema we'll
-- work with.
--
-- > # \d author
-- >   Column   |  Type   | Nullable 
-- > -----------+---------+----------
-- >  author_id | integer | not null 
-- >  name      | text    | not null
-- >  url       | text    |        
--
-- > # \d project
-- >   Column   |  Type   | Nullable 
-- > -----------+---------+----------
-- >  author_id | integer | not null 
-- >  name      | text    | not null
--
-- Our schema consists of two tables - @author@ and @project@. An @author@ has
-- zero+ projects, a name and (maybe) an associated website. Each project has
-- an author and a name.
--
-- Now that we've seen our schema, we can begin writing a mapping in Rel8. The
-- idiomatic way to map a table is to use a record that is parameterised by a
-- particular interpretation functor, and to define each field with 'Column'.
-- For this type to be usable with Rel8 we need it to be an instance of
-- 'HigherKindedTable', which can be derived with a combination of
-- @DeriveAnyClass@ and @DeriveGeneric@.
--
-- Following these steps, we have:
--
-- > data Author f = Author
-- >   { authorId :: Column f Int64
-- >   , name     :: Column f Text
-- >   , url      :: Column f (Maybe Text)
-- >   } deriving (Generic, HigherKindedTable)
--
-- However, cautious readers might notice a problem with this - in particular,
-- with the type of the @authorId@ field. While @Int64@ is correct, it's not
-- the best type. If we had other identifier types in our project, it would be
-- too easy to accidentally mix them up and create nonsensical joins. Instead,
-- it's a good idea to a create a @newtype@ for each identifier type, allowing
-- them to be distinct. 
--
-- Rel8 makes this easy - we can just use @GeneralizedNewtypeDeriving@:
--
-- > newtype AuthorId = AuthorId { toInt64 :: Int64 } deriving (DBType)
--
-- Now we can write our final schema mapping:
--
-- >>> :set -XGeneralizedNewtypeDeriving -XDeriveAnyClass -XDerivingStrategies -XDeriveGeneric -XStandaloneDeriving -XTypeFamilies
--
-- >>> :{
-- newtype AuthorId = AuthorId { toInt64 :: Int64 } deriving newtype (DBEq, DBType, Eq, Show)
-- :}
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
-- >>> deriving stock instance f ~ Identity => Show (Author f)
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
-- >>> deriving stock instance f ~ Identity => Show (Project f)
--
-- These data types describe the structural mapping of the tables, but we also
-- need to specify a 'TableSchema'. A @TableSchema@ contains the name of the
-- table and the name of all columns in the table, which will ultimately allow
-- us to @SELECT@ and @INSERT@ rows for these tables.
--
-- As an aside, you might be wondering why this information isn't in the
-- definitions of @Author@ and @Project@ above. Rel8 decouples @TableSchema@
-- from the data types themselves, as not all tables you define will
-- necessarily have a schema. For example, Rel8 allows you to define helper
-- types to simplify the types of queries - these tables only exist at query
-- time, but there is no corresponding base table. We'll see more on this idea
-- later!
--
-- To define a @TableSchema@, we just need to fill construct appropriate
-- @TableSchema@ values. When it comes to the @tableColumns@ field, we just use
-- our data types above, and set each field to the name of the column that it
-- maps to:
--
-- >>> :set -XOverloadedStrings
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
-- With these table definitions, we can now start writing some queries!
--
-- >>> :set -XBlockArguments -XDerivingVia -XTypeApplications -XDuplicateRecordFields
-- >>> c <- Database.PostgreSQL.Simple.connectPostgreSQL . Data.ByteString.Char8.pack =<< System.Environment.getEnv "TEST_DATABASE_URL"
-- >>> Database.PostgreSQL.Simple.Transaction.begin c

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
-- @SELECT FROM@ statement. To select rows from a table, we use 'each', and
-- supply a @TableSchema@. To select all projects, we can write:
-- 
-- >>> :t each projectSchema
-- each projectSchema :: Query (Project Expr)
-- 
-- Notice that @each@ gives us a @Query@ that yields @Project Expr@ rows. To
-- see what this means, let's have a look at a single field of a @Project
-- Expr@:
-- 
-- >>> let aProjectExpr = undefined :: Project Expr
-- >>> :t projectAuthorId aProjectExpr
-- projectAuthorId aProjectExpr :: Expr AuthorId
-- 
-- We defined @projectAuthorId@ as @Column f AuthorId@, but here @f@ is @Expr@,
-- and @Column Expr AuthorId@ reduces to @Expr AuthorId@. We'll see more about
-- @Expr@ soon, but you can think of @Expr a@ as "SQL expressions of type @a@".
-- 
-- To execute this @Query@, we pass it off to 'select':
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
-- >>> select c (each projectSchema)
-- [Project {projectAuthorId = 1, projectName = "rel8"},Project {projectAuthorId = 2, projectName = "aeson"},Project {projectAuthorId = 2, projectName = "text"}]
-- 
-- Cool!
--
-- 'each' is the equivalent of a @SELECT *@ query, but sometimes we're only
-- interested in a subset of the columns of a table. To restrict the returned
-- columns, we can specify a projection by using 'Query's @Functor@ instance:
--
-- >>> select c $ projectName <$> each projectSchema
-- ["rel8","aeson","text"]

-- $guideJoins
-- 
-- A very common operation in relational databases is to take the @JOIN@ of
-- multiple tables. Rel8 doesn't have a specific join operation, but we can
-- recover the functionality of a join by selecting all rows of two tables, and
-- then using 'where_' to filter them.
--
-- To see why this works, first let's look at taking the product of two tables.
-- We can do this by simply calling 'each' twice:
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
-- returned rows:
-- 
-- > select c $ do
-- >   author  <- each authorSchema
-- >   project <- each projectSchema
-- >   where_ $ projectAuthorId project ==. authorId author
-- >   return (project, author)
--
-- Doing this every time you need a join can obscure the meaning of the query
-- you're writing, so a good practice is to introduce specialised functions for
-- the particular joins in your database. In our case, this would be:
--
-- >>> :{
-- projectsForAuthor :: Author Expr -> Query (Project Expr)
-- projectsForAuthor a = each projectSchema >>= filter \p ->
--   projectAuthorId p ==. authorId a
-- :}
--
-- Now we can simplify our query to just:
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
-- follow a similar approach, but use the 'optional' query transformer to allow
-- for the possibility of the join to fail.
--
-- In our test database, we can see that there's another author:
--
-- >>> select c $ authorName <$> each authorSchema
-- ["Ollie","Bryan O'Sullivan","Emily Pillmore"]
--
-- Emily wasn't returned in our earlier query because - in our database - she
-- doesn't have any registered projects. We can record this partiality in our
-- original query by simply wrapping the @projectsForAuthor@ call with
-- 'optional':
--
-- >>> :{
-- mapM_ print =<< select c do
--   author <- each authorSchema
--   mproject <- optional (projectsForAuthor author)
--   return (authorName author, projectName <$> mproject)
-- :}
-- ("Ollie",Just "rel8")
-- ("Bryan O'Sullivan",Just "aeson")
-- ("Bryan O'Sullivan",Just "text")
-- ("Emily Pillmore",Nothing)

-- $guideAggregation
--
-- Another common operation in SQL is to aggregate data. Aggregations are
-- operations like @sum@ and @count@, and Rel8 also supports this. To perform
-- an aggregation, we can use the 'aggregate' function, which takes a 'Query'
-- of aggregated expressions, runs the aggregation, and returns you back result
-- rows.
--
-- To start, let's look at a simple aggregation that tells us how many projects
-- exist:
--
-- TODO
--
-- Rel8 is also capable of aggregating multiple rows into a single row by
-- concatenating all rows as a list. This aggregation allows us to break free
-- of the row-orientated nature of SQL and write queries that return tree-like
-- structures. Earlier we saw an example of returning authors with their
-- projects, but the query didn't do a great job of describing the relationship
-- between an author and their projects.
--
-- Let's look again at a query that returns authors and their projects, and
-- focus on the type of that query:
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
-- :}
--
-- >>> :t select c authorsAndProjects
-- select c authorsAndProjects 
--   :: MonadIO m => m [(Author Identity, Project Identity)]
--
-- Our query gives us a single list of pairs of authors and projects. However,
-- with our domain knowledge of the schema, this isn't a great type - what we'd
-- rather have is a list of pairs of authors and /lists/ of projects:
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
class Typeable a => DBType (a :: Type) where
  -- | Lookup the type information for the type @a@.
  typeInformation :: DatabaseType a


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
-- @json@ values):
-- 
-- >>> import Data.String (fromString)
-- >>> import Data.Aeson (Value, encode)
-- >>> import Database.PostgreSQL.Simple (query_, fromOnly)
-- >>> fmap (Data.Aeson.encode @Value . fromOnly) <$> query_ c (fromString $ showQuery $ pure $ lit Pet{ petName = "Yoshi", petAge = 4 })
-- ["{\"petAge\":4,\"petName\":\"Yoshi\"}"]
newtype JSONEncoded a = JSONEncoded { fromJSONEncoded :: a }


instance (FromJSON a, ToJSON a, Typeable a) => DBType (JSONEncoded a) where
  typeInformation = parseDatabaseType f g typeInformation
    where
      f = fmap JSONEncoded . parseEither parseJSON
      g = toJSON . fromJSONEncoded


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
-- @text@):
-- 
-- >>> import Data.Text (Text)
-- >>> import Database.PostgreSQL.Simple (query_, fromOnly)
-- >>> fmap (fromOnly @Text) <$> query_ c (fromString $ showQuery $ pure $ lit Red)
-- ["Red"]
newtype ReadShow a = ReadShow { fromReadShow :: a }


-- | A @DatabaseType@ describes how to encode and decode a Haskell type to and
-- from database queries. The @typeName@ is the name of the type in the
-- database, which is used to accurately type literals. 
data DatabaseType (a :: Type) = DatabaseType
  { encode :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as a SQL expression.
  , typeName :: String
    -- ^ The name of the SQL type.
  , decoder :: Decoder a
    -- ^ How to deserialize a single result back to Haskell.
  }


-- | A @Decoder@ describes how to parse PostgreSQL results back to Haskell
-- values. A @Decoder@ specifies two routines - a method to decode from
-- PostgreSQL's text representation, and a method to decode from PostgreSQL's
-- @to_json@ function.
data Decoder a = Decoder
  { decodeBytes :: Maybe ByteString -> Conversion a
    -- ^ How to decode from the text representation. This is used for the
    -- majority of queries.
  , decodeJSON :: Value -> Conversion a
    -- ^ How to decode values that have been passed through @to_json@. This is
    -- used for aggregate tables like 'ListTable' and 'NonEmptyTable'.
  }


instance Functor Decoder where
  fmap f Decoder{ decodeBytes, decodeJSON } = Decoder
    { decodeBytes = fmap f <$> decodeBytes
    , decodeJSON = fmap f <$> decodeJSON
    }


-- | Enrich a 'DatabaseType' with the ability to parse @null@.
acceptNull :: Decoder a -> Decoder (Maybe a)
acceptNull Decoder{ decodeJSON, decodeBytes } = Decoder
  { decodeJSON = \case
      Data.Aeson.Null -> pure Nothing
      otherJSON       -> Just <$> decodeJSON otherJSON
  , decodeBytes = \case
      Nothing -> pure Nothing
      bytes   -> Just <$> decodeBytes bytes
  }


-- | Apply a parser to a decoder.
parseDecoder :: (a -> Either String b) -> Decoder a -> Decoder b
parseDecoder f Decoder{ decodeBytes, decodeJSON } = Decoder
  { decodeBytes = \x -> decodeBytes x >>= either (error "TODO") return . f
  , decodeJSON = \x -> decodeJSON x >>= either (error "TODO") return . f
  }


bytestringDecoder :: Decoder ByteString
bytestringDecoder = Decoder
  { decodeBytes = \case
      Just bytes -> pure bytes
  , decodeJSON = \case
      Data.Aeson.String s -> pure $ encodeUtf8 s
  }


monolit :: DatabaseType a -> a -> Expr a
monolit databaseType = fromPrimExpr . encode databaseType


-- | Simultaneously map over how a type is both encoded and decoded, while
-- retaining the name of the type. This operation is useful if you want to
-- essentially @newtype@ another 'DatabaseType'.
-- 
-- The mapping is required to be total. If you have a partial mapping, see
-- 'parseDatabaseType'.
mapDatabaseType :: (a -> b) -> (b -> a) -> DatabaseType a -> DatabaseType b
mapDatabaseType aToB bToA DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = encode . bToA
  , decoder = aToB <$> decoder
  , typeName
  }


-- | Apply a parser to a 'DatabaseType'.
-- 
-- This can be used if the data stored in the database should only be subset of
-- a given 'DatabaseType'. The parser is applied when deserializing rows
-- returned - the encoder assumes that the input data is already in the
-- appropriate form.
-- 
-- One example where this may be useful is with a database that stores data in
-- some legacy encoding:
-- 
-- >>> import Data.Text (Text)
-- >>> import Database.PostgreSQL.Simple (query_, fromOnly)
-- 
-- >>> data Color = Red | Green | Blue
-- >>> :{
-- instance DBType Color where
--   typeInformation = parseDatabaseType parseLegacy toLegacy typeInformation
--     where
--       parseLegacy :: Text -> Either String Color
--       parseLegacy "red"   = Right Red
--       parseLegacy "green" = Right Green
--       parseLegacy _       = Left "Unexpected Color"
--       toLegacy Red   = "red"
--       toLegacy Green = "green"
-- :}
-- 
-- >>> fmap (fromOnly @Text) <$> query_ c (fromString $ showQuery $ pure $ lit Red)
-- ["red"]
parseDatabaseType :: (a -> Either String b) -> (b -> a) -> DatabaseType a -> DatabaseType b
parseDatabaseType aToB bToA DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = encode . bToA
  , decoder = parseDecoder aToB decoder
  , typeName
  }


-- | Database column types that can be compared for equality in queries.
-- 
-- Usually, this means producing an expression using the (overloaded) @=@
-- operator, but types can provide a more elaborate expression if necessary.
-- 
-- [ @DBEq@ with @newtype@s ]
-- 
-- Like with 'Rel8.DBType', @DBEq@ plays well with generalized newtype
-- deriving.  The example given for @DBType@ added a @UserId@ @newtype@, but
-- without a @DBEq@ instance won't actually be able to use that in joins or
-- where-clauses, because it lacks equality. We can add this by changing our
-- @newtype@ definition to:
-- 
-- >>> newtype UserId = UserId { toInt32 :: Int32 } deriving newtype (DBType, DBEq)
-- 
-- This will re-use the equality logic for @Int32@, which is to just use the
-- @=@ operator.
-- 
-- [ @DBEq@ with @DeriveAnyType@ ]
-- 
-- You can also use @DBEq@ with the @DeriveAnyType@ extension to easily add
-- equality to your type, assuming that @=@ is sufficient on @DBType@ encoded
-- values. Extending the example from 'Rel8.ReadShow''s 'Rel8.DBType' instance,
-- we could add equality to @Color@ by writing:
-- 
-- >>> :{
-- data Color = Red | Green | Blue | Purple | Gold
--   deriving (Generic, Show, Read, DBEq)
--   deriving DBType via ReadShow Color
-- :}
-- 
-- This means @Color@s will be treated as the literal strings @"Red"@,
-- @"Green"@, etc, in the database, and they can be compared for equality by
-- just using @=@.
class DBType a => DBEq (a :: Type) where
  eqExprs :: Expr a -> Expr a -> Expr Bool
  eqExprs = binExpr (Opaleye.:==)


-- | Typed SQL expressions
newtype Expr (a :: Type) = Expr { toPrimExpr :: Opaleye.PrimExpr }


-- | It is assumed that any Haskell types that have a 'Num' instance also have
-- the corresponding operations in the database. Hence, Num a => Num (Expr a).
-- *However*, if this is not the case, you should `newtype` the Haskell type
-- and avoid providing a 'Num' instance, or you may write be able to write
-- ill-typed queries!
instance (DBType a, Num a) => Num (Expr a) where
  a + b = columnToExpr (Opaleye.binOp (Opaleye.:+) (exprToColumn a) (exprToColumn b))
  a * b = columnToExpr (Opaleye.binOp (Opaleye.:*) (exprToColumn a) (exprToColumn b))
  abs = function "abs"
  signum = columnToExpr @Opaleye.PGInt8 . signum . exprToColumn
  fromInteger = lit . fromInteger
  negate = columnToExpr @Opaleye.PGInt8 . negate . exprToColumn


instance (DBType a, Fractional a) => Fractional (Expr a) where
  a / b = columnToExpr (Opaleye.binOp (Opaleye.:/) (exprToColumn a) (exprToColumn b))
  fromRational = lit . fromRational


exprToColumn :: Expr a -> Opaleye.Column b
exprToColumn (Expr a) = Opaleye.Column a


columnToExpr :: Opaleye.Column b -> Expr a
columnToExpr (Opaleye.Column a) = Expr a


-- | Cast an @Expr@ from one type to another.
unsafeCastExpr :: String -> Expr a -> Expr b
unsafeCastExpr t (Expr x) = Expr $ Opaleye.CastExpr t x


-- | Unsafely treat an 'Expr' that returns @a@s as returning @b@s.
unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr x) = Expr x


-- | Construct a SQL expression from some literal text. The provided literal
-- will be interpolated exactly as specified with no escaping.
unsafeLiteral :: forall a. String -> Expr a
unsafeLiteral = columnToExpr @a @a . Opaleye.Column . Opaleye.ConstExpr . Opaleye.OtherLit


-- | Construct an expression by applying an infix binary operator to two
-- operands.
binaryOperator :: String -> Expr a -> Expr b -> Expr c
binaryOperator op (Expr a) (Expr b) = Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b


-- | Like 'maybe', but to eliminate @null@.
--
-- >>> select c $ pure $ null_ 0 id (nullExpr :: Expr (Maybe Int32))
-- [0]
--
-- >>> select c $ pure $ null_ 0 id (lit (Just 42) :: Expr (Maybe Int32))
-- [42]
null_ :: DBType b => Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
null_ whenNull f a = ifThenElse_ (isNull a) whenNull (f (retype a))


-- | Like 'isNothing', but for @null@.
--
-- >>> select c $ pure $ isNull (nullExpr :: Expr (Maybe Int32))
-- [True]
--
-- >>> select c $ pure $ isNull (lit (Just 42) :: Expr (Maybe Int32))
-- [False]
isNull :: Expr (Maybe a) -> Expr Bool
isNull = fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNull . toPrimExpr


-- | Corresponds to SQL @null@.
nullExpr :: DBType a => Expr (Maybe a)
nullExpr = lit Nothing


-- | Lift an expression that's not null to a type that might be @null@. This is
-- an identity operation in terms of any generated query, and just modifies the
-- query's type.
liftNull :: Expr a -> Expr ( Maybe a )
liftNull = retype


-- | Lift an operation on non-@null@ values to an operation on possibly @null@
-- values.
-- 
-- @mapNull@ requires that the supplied function "preserves nulls", as no
-- actual case analysis is done (instead the @Expr (Maybe a)@ is simply retyped
-- and assumed to not be @null@). In most cases, this is true, but this
-- contract can be violated with custom functions.
mapNull :: (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
mapNull f = retype . f . retype


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
liftOpNull :: (Expr a -> Expr b -> Expr c) -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
liftOpNull f a b = retype (f (retype a) (retype b))


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
catMaybe :: Expr (Maybe a) -> Query (Expr a)
catMaybe e = catMaybeTable $ MaybeTable nullTag (unsafeCoerceExpr e)
  where
    nullTag = ifThenElse_ (isNull e) (lit Nothing) (lit (Just False))


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
Expr a &&. Expr b = Expr $ Opaleye.BinExpr Opaleye.OpAnd a b


-- | Fold @AND@ over a collection of expressions.
--  
-- >>> select c $ pure $ and_ [ lit True ==. lit False, lit False, lit True ]
-- [False]
--  
-- >>> select c $ pure $ and_ []
-- [True]
and_ :: Foldable f => f (Expr Bool) -> Expr Bool
and_ = foldl' (&&.) (lit True)


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
Expr a ||. Expr b = Expr $ Opaleye.BinExpr Opaleye.OpOr a b


-- | Fold @OR@ over a collection of expressions.
-- 
-- >>> select c $ pure $ or_ [ lit True ==. lit False, lit False, lit True ]
-- [True]
--  
-- >>> select c $ pure $ or_ []
-- [False]
or_ :: Foldable f => f (Expr Bool) -> Expr Bool
or_ = foldl' (||.) (lit False)


-- | The SQL @NOT@ operator.
--
-- >>> select c $ pure $ not_ $ lit True
-- [False]
--
-- >>> select c $ pure $ not_ $ lit False
-- [True]
not_ :: Expr Bool -> Expr Bool
not_ (Expr a) = Expr $ Opaleye.UnExpr Opaleye.OpNot a


-- | Like the SQL @IN@ operator, but implemented by folding over a list with
-- '==.' and '||.'.
--
-- >>> select c $ return $ lit (5 :: Int32) `in_` [ lit x | x <- [1..5] ]
-- [True]
--
-- >>> select c $ return $ lit (42 :: Int32) `in_` [ lit x | x <- [1..5] ]
-- [False]
in_ :: DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> b ||. x ==. y) (lit False)


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
ifThenElse_ :: Table Expr a => Expr Bool -> a -> a -> a
ifThenElse_ bool whenTrue = case_ [(bool, whenTrue)]


-- | The class of database tables (containing one or more columns) that can be
-- compared for equality as a whole.
class Table Expr a => EqTable a where
  -- | Compare two tables or expressions for equality.
  (==.) :: a -> a -> Expr Bool


-- | The @Function@ type class is an implementation detail that allows
-- @function@ to be polymorphic in the number of arguments it consumes.
class Function arg res where
  -- We do need 'applyArgument', but if we don't specify this and let GHC infer
  -- the minimal contract, it leaks out into documentation.
  {-# minimal #-}

  -- | Build a function of multiple arguments.
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res


instance arg ~ Expr a => Function arg (Expr res) where
  applyArgument mkExpr (Expr a) = Expr $ mkExpr [a]


instance (arg ~ Expr a, Function args res) => Function arg (args -> res) where
  applyArgument f (Expr a) = applyArgument (f . (a :))


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
-- power :: Expr Float -> Expr Float -> Expr Float
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
-- sqlPi :: Expr Float
-- sqlPi = nullaryFunction "pi"
-- :}
-- 
-- >>> select c $ pure $ sqlPi
-- [3.1415927]
nullaryFunction :: String -> Expr a
nullaryFunction name = Expr (Opaleye.FunExpr name [])


-- | Types that represent SQL tables.
-- 
-- You generally should not need to derive instances of this class manually, as
-- writing higher-kinded data types is usually more convenient. See also:
-- 'HigherKindedTable'.
class HTable (Columns t) => Table (context :: Type -> Type) (t :: Type) | t -> context where
  type Columns t :: KContext -> Type

  toColumns :: t -> Columns t (Context context)
  fromColumns :: Columns t (Context context) -> t


data KContext where
  Context :: (Type -> Type) -> KContext


type Context = 'Context


-- | Higher-kinded data types.
--
-- Higher-kinded data types are data types of the pattern:
--
-- @
-- data MyType f =
--   MyType { field1 :: Column f T1 OR HK1 f
--          , field2 :: Column f T2 OR HK2 f
--          , ...
--          , fieldN :: Column f Tn OR HKn f
--          }
-- @
-- 
-- where @Tn@ is any Haskell type, and @HKn@ is any higher-kinded type.
-- 
-- That is, higher-kinded data are records where all fields in the record are
-- all either of the type @Column f T@ (for any @T@), or are themselves
-- higher-kinded data:
-- 
-- [Nested]
-- 
-- @
-- data Nested f =
--   Nested { nested1 :: MyType f
--          , nested2 :: MyType f
--          }
-- @
-- 
-- The @HigherKindedTable@ type class is used to give us a special mapping
-- operation that lets us change the type parameter @f@.
-- 
-- [Supplying @HigherKindedTable@ instances]
-- 
-- This type class should be derived generically for all table types in your
-- project. To do this, enable the @DeriveAnyType@ and @DeriveGeneric@ language
-- extensions:
-- 
-- @
-- \{\-\# LANGUAGE DeriveAnyClass, DeriveGeneric #-\}
-- 
-- data MyType f = MyType { fieldA :: Column f T }
--   deriving ( GHC.Generics.Generic, HigherKindedTable )
-- @
class HTable (t :: KContext -> Type) where
  type HField t = (field :: Type -> Type) | field -> t
  type HConstrainTable t (c :: Type -> Constraint) :: Constraint

  hfield :: t (Context f) -> HField t x -> f x
  htabulate :: forall f. (forall x. HField t x -> f x) -> t (Context f)
  htraverse :: forall f g m. Applicative m => (forall x. f x -> m (g x)) -> t (Context f) -> m (t (Context g))
  hdbtype :: t (Context DatabaseType)

  type HField t = GenericHField t
  type HConstrainTable t c = HConstrainTable (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context IsColumn)) ()))) c

  default hfield
    :: forall f x
     . ( Generic (t (Context f))
       , HField t ~ GenericHField t
       , Congruent (WithShape f (Rep (t (Context IsColumn))) (Rep (t (Context f)) ())) (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))
       , HField (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))) ~ HField (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context IsColumn)) ())))
       , HTable (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ())))
       , Table f (WithShape f (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))
       )
    => t (Context f) -> HField t x -> f x
  hfield x (GenericHField i) =
    hfield (toColumns (WithShape @f @(Rep (t (Context IsColumn))) (GHC.Generics.from @_ @() x))) i

  default htabulate
    :: forall f
     . ( Generic (t (Context f))
       , HField t ~ GenericHField t
       , Congruent (WithShape f (Rep (t (Context IsColumn))) (Rep (t (Context f)) ())) (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))
       , HField (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))) ~ HField (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context IsColumn)) ())))
       , HTable (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ())))
       , Table f (WithShape f (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))
       )
    => (forall a. HField t a -> f a) -> t (Context f)
  htabulate f =
    to @_ @() $ forgetShape @f @(Rep (t (Context IsColumn))) $ fromColumns $ htabulate (f . GenericHField)

  default htraverse
    :: forall f g m
     . ( Applicative m
       , Generic (t (Context f))
       , Generic (t (Context g))
       , Congruent (WithShape f (Rep (t (Context IsColumn))) (Rep (t (Context f)) ())) (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))
       , HTable (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ())))
       , Table f (WithShape f (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))
       , Table g (WithShape g (Rep (t (Context IsColumn))) (Rep (t (Context g)) ()))
       , Congruent (WithShape g (Rep (t (Context IsColumn))) (Rep (t (Context g)) ())) (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context f)) ()))
       )
    => (forall a. f a -> m (g a)) -> t (Context f) -> m (t (Context g))
  htraverse f x =
    fmap (to @_ @() . forgetShape @g @(Rep (t (Context IsColumn))) . fromColumns)
      $ htraverse f
      $ toColumns
      $ WithShape @f @(Rep (t (Context IsColumn)))
      $ GHC.Generics.from @_ @() x

  default hdbtype ::
    ( Generic (t (Context DatabaseType))
    , Table DatabaseType (WithShape DatabaseType (Rep (t (Context IsColumn))) (Rep (t (Context DatabaseType)) ()))
    )
    => t (Context DatabaseType)
  hdbtype =
    to @_ @() $
      forgetShape @DatabaseType @(Rep (t (Context IsColumn))) $
        fromColumns $
          hdbtype @(Columns (WithShape DatabaseType (Rep (t (Context IsColumn))) (Rep (t (Context DatabaseType)) ())))


hmap :: HTable t => (forall x. f x -> g x) -> t (Context f) -> t (Context g)
hmap f t = htabulate $ f <$> hfield t


hzipWith :: HTable t => (forall x. f x -> g x -> h x) -> t (Context f) -> t (Context g) -> t (Context h)
hzipWith f t u = htabulate $ f <$> hfield t <*> hfield u


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


-- | The @Column@ type family should be used to indicate which fields of your
-- data types are single columns in queries. This type family has special
-- support when a query is executed, allowing you to use a single data type for
-- both query data and rows decoded to Haskell.
-- 
-- To understand why this type family is special, let's consider a simple
-- higher-kinded data type of Haskell packages:
-- 
-- >>> :{
-- data Package f = Package
--   { packageName   :: Column f Text
--   , packageAuthor :: Column f Text
--   }
-- :}
-- 
-- In queries, @f@ will be some type of 'Expr', and @Column Expr a@ reduces to
-- just @Expr a@:
-- 
-- >>> :t packageName (undefined :: Package Expr)
-- packageName (undefined :: Package Expr) :: Expr Text
-- 
-- When we 'select' queries of this type, @f@ will be instantiated as
-- @Identity@, at which point all wrapping entire disappears:
-- 
-- >>> :t packageName (undefined :: Package Identity)
-- packageName (undefined :: Package Identity) :: Text
-- 
-- In @rel8@ we try hard to always know what @f@ is, which means holes should
-- mention precise types, rather than the @Column@ type family. You should only
-- need to be aware of the type family when defining your table types.
type family Column (context :: (Type -> Type)) (a :: Type) :: Type where
  Column Identity a = a
  Column f a        = f a


class HTable (GRep t) => HigherKindedTable (t :: (Type -> Type) -> Type) where
  type GRep t :: KContext -> Type
  type GRep t = GColumns (Rep (t Expr))

  toExprs :: t Expr -> GRep t (Context Expr)
  fromExprs :: GRep t (Context Expr) -> t Expr

  default toExprs
    :: ( GColumns (Rep (t Expr)) ~ GRep t
       , HigherKindedTableImpl Expr (Rep (t Expr))
       , Generic (t Expr)
       )
    => t Expr -> GRep t (Context Expr)
  toExprs = ghigherKindedTo @Expr @(Rep (t Expr)) . GHC.Generics.from @_ @()

  default fromExprs
    :: ( GColumns (Rep (t Expr)) ~ GRep t
       , HigherKindedTableImpl Expr (Rep (t Expr))
       , Generic (t Expr)
       )
    => GRep t (Context Expr) -> t Expr
  fromExprs = to @_ @() . ghigherKindedFrom @Expr @(Rep (t Expr))

  toColumnSchemas :: t ColumnSchema -> GRep t (Context ColumnSchema)
  fromColumnSchemas :: GRep t (Context ColumnSchema) -> t ColumnSchema

  default toColumnSchemas
    :: ( GColumns (Rep (t ColumnSchema)) ~ GRep t
       , HigherKindedTableImpl ColumnSchema (Rep (t ColumnSchema))
       , Generic (t ColumnSchema)
       )
    => t ColumnSchema -> GRep t (Context ColumnSchema)
  toColumnSchemas = ghigherKindedTo @ColumnSchema @(Rep (t ColumnSchema)) . GHC.Generics.from @_ @()

  default fromColumnSchemas
    :: ( GColumns (Rep (t ColumnSchema)) ~ GRep t
       , HigherKindedTableImpl ColumnSchema (Rep (t ColumnSchema))
       , Generic (t ColumnSchema)
       )
    => GRep t (Context ColumnSchema) -> t ColumnSchema
  fromColumnSchemas = to @_ @() . ghigherKindedFrom @ColumnSchema @(Rep (t ColumnSchema))

  glit :: t Identity -> t Expr
  default glit
    :: ( Generic (t Identity)
       , Generic (t Expr)
       , GSerializable (Rep (t Expr)) (Rep (t Identity))
       )
    => t Identity -> t Expr
  glit = to @_ @() . glitImpl @(Rep (t Expr)) @(Rep (t Identity)) . GHC.Generics.from @_ @()

  growParser :: Applicative f
    => (forall a. Decoder a -> x -> Conversion (f a))
    -> GRep t (Context (Const x))
    -> Conversion (f (t Identity))
  default growParser
    :: ( Generic (t Identity)
       , GSerializable (Rep (t Expr)) (Rep (t Identity))
       , Applicative f
       , GColumns (Rep (t Expr)) ~ GRep t
       )
    => (forall a. Decoder a -> x -> Conversion (f a))
    -> GRep t (Context (Const x))
    -> Conversion (f (t Identity))
  growParser f xs = fmap (to @_ @()) <$> growParserImpl @(Rep (t Expr)) @(Rep (t Identity)) f xs


class GSerializable (expr :: Type -> Type) (haskell :: Type -> Type) where
  glitImpl :: haskell x -> expr x

  growParserImpl :: Applicative f
    => (forall a. Decoder a -> t -> Conversion (f a))
    -> GColumns expr (Context (Const t))
    -> Conversion (f (haskell x))


instance GSerializable f f' => GSerializable (M1 i c f) (M1 i' c' f') where
  glitImpl = M1 . glitImpl @f @f' . unM1
  growParserImpl f xs = fmap M1 <$> growParserImpl @f @f' f xs


instance (GSerializable f f', GSerializable g g') => GSerializable (f :*: g) (f' :*: g') where
  glitImpl (x :*: y) = glitImpl @f @f' x :*: glitImpl @g @g' y
  growParserImpl f (HPair x y) = liftA2 (liftA2 (:*:)) (growParserImpl @f @f' f x) (growParserImpl @g @g' f y)


instance Serializable expr haskell => GSerializable (K1 i expr) (K1 i haskell) where
  glitImpl = K1 . lit . unK1
  growParserImpl f xs = fmap K1 <$> rowParser @expr @haskell f xs


class HigherKindedTableImpl (context :: Type -> Type) (rep :: Type -> Type) where
  type GColumns rep :: KContext -> Type
  ghigherKindedTo :: rep x -> GColumns rep (Context context)
  ghigherKindedFrom :: GColumns rep (Context context) -> rep x


instance HigherKindedTableImpl context f => HigherKindedTableImpl context (M1 i c f) where
  type GColumns (M1 i c f) = GColumns f
  ghigherKindedTo = ghigherKindedTo @context @f . unM1
  ghigherKindedFrom = M1 . ghigherKindedFrom @context @f


instance (HigherKindedTableImpl context f, HigherKindedTableImpl context g) => HigherKindedTableImpl context (f :*: g) where
  type GColumns (f :*: g) = HPair (GColumns f) (GColumns g)
  ghigherKindedTo (x :*: y) = HPair (ghigherKindedTo @context @f x) (ghigherKindedTo @context @g y)
  ghigherKindedFrom (HPair x y) = ghigherKindedFrom @context @f x :*: ghigherKindedFrom @context @g y


instance Table context a => HigherKindedTableImpl context (K1 i a) where
  type GColumns (K1 i a) = Columns a
  ghigherKindedTo (K1 a) = toColumns a
  ghigherKindedFrom = K1 . fromColumns


class Helper f t where
  helperTo :: t f -> GRep t (Context f)
  helperFrom :: GRep t (Context f) -> t f


instance (x ~ f, HigherKindedTable t, Helper f t) => Table f (t x) where
  type Columns (t x) = GRep t
  toColumns = helperTo
  fromColumns = helperFrom


instance HigherKindedTable t => Helper Expr t where
  helperTo = toExprs
  helperFrom = fromExprs


instance HigherKindedTable t => Helper ColumnSchema t where
  helperTo = toColumnSchemas
  helperFrom = fromColumnSchemas


-- | We would like to write a default type
-- 
-- @
-- type HField t = HField (Columns (Rep ..))
-- @
-- 
-- but this will violate the injectivity of the HField type (as there might be
-- two 't's with the same 'Rep'). This newtype restores that injectivity.
newtype GenericHField t a where
  GenericHField :: HField (Columns (WithShape IsColumn (Rep (t (Context IsColumn))) (Rep (t (Context IsColumn)) ()))) a -> GenericHField t a


-- | To facilitate generic deriving for higher-kinded table, we work through
-- Tables and the WithShape wrapper. The idea is that whenever we have a 't f',
-- we can view this as a specific Table instance for Rep (t f). However, the
-- story is not quite as simple as a typical generic traversal. For higher
-- kinded tables, we expect one of two things to be true for all fields:
-- 
-- 1. The field is a Column application. In this case we know that we've got a
-- single DBType, and we need to have a single HIdentity in Columns.
-- 
-- 2. The field is a nested Table. In this case, we need to concatenate all
-- Columns of this Table into the parent Table.
-- 
-- To distinguish between these two cases, we apply t to a special IsColumn
-- tag.  This controlled application lets us observe more information at each
-- K1 node in the rep.
-- 
-- However, there's /another/ complication! If we have 't Identity', then any
-- Column fields will vanish, but we'll be unable to easily see this in the K1
-- node. To deal with this, we also explicitly track the context in the
-- 'WithShape' type.
newtype WithShape (context :: Type -> Type) (shape :: Type -> Type) a = WithShape { forgetShape :: a }


-- | A special functor for use with Column to see the structure of a
-- higher-kinded table.
data IsColumn a


instance (context ~ context', Table context (WithShape context f (g a))) => Table context (WithShape context' (M1 i c f) (M1 i c g a)) where
  type Columns (WithShape context' (M1 i c f) (M1 i c g a)) = Columns (WithShape context' f (g a))
  toColumns = toColumns . WithShape @context @f . unM1 . forgetShape
  fromColumns = WithShape . M1 . forgetShape @context @f . fromColumns


instance (context ~ context', Table context (WithShape context shapeL (l a)), Table context (WithShape context shapeR (r a))) => Table context (WithShape context' (shapeL :*: shapeR) ((:*:) l r a)) where
  type Columns (WithShape context' (shapeL :*: shapeR) ((:*:) l r a)) =
    HPair
      (Columns (WithShape context' shapeL (l a)))
      (Columns (WithShape context' shapeR (r a)))

  toColumns (WithShape (x :*: y)) = HPair (toColumns (WithShape @context @shapeL x)) (toColumns (WithShape @context @shapeR y))
  fromColumns (HPair x y) = WithShape $ forgetShape @context @shapeL (fromColumns x) :*: forgetShape @context @shapeR (fromColumns y)


instance (context ~ context', K1Helper (IsColumnApplication shape) context shape b) => Table context (WithShape context' (K1 i shape) (K1 i b x)) where
  type Columns (WithShape context' (K1 i shape) (K1 i b x)) = K1Columns (IsColumnApplication shape) shape b
  toColumns = toColumnsHelper @(IsColumnApplication shape) @context @shape @b . unK1 . forgetShape
  fromColumns = WithShape . K1 . fromColumnsHelper @(IsColumnApplication shape) @context @shape @b


type family IsColumnApplication (a :: Type) :: Bool where
  IsColumnApplication (IsColumn _) = 'True
  IsColumnApplication _            = 'False


-- | This helper lets us distinguish between 'fieldN :: Column f Int' and
-- 'nestedTable :: t f' fields in higher kinded tables. 
class (isColumnApplication ~ IsColumnApplication shape, HTable (K1Columns isColumnApplication shape a)) => K1Helper (isColumnApplication :: Bool) (context :: Type -> Type) (shape :: Type) (a :: Type) where
  type K1Columns isColumnApplication shape a :: KContext -> Type
  toColumnsHelper :: a -> K1Columns isColumnApplication shape a (Context context)
  fromColumnsHelper :: K1Columns isColumnApplication shape a (Context context) -> a


instance (Table f a, IsColumnApplication shape ~ 'False) => K1Helper 'False f shape a where
  type K1Columns 'False shape a = Columns a
  toColumnsHelper = toColumns
  fromColumnsHelper = fromColumns


instance (DBType a, g ~ f a) => K1Helper 'True f (IsColumn a) g where
  type K1Columns 'True (IsColumn a) g = HIdentity a
  toColumnsHelper = HIdentity
  fromColumnsHelper = unHIdentity


-- | Any 'HigherKindedTable' is also a 'Table'.
instance (HTable t, f ~ g) => Table f (t (Context g)) where
  type Columns (t (Context g)) = t
  toColumns = id
  fromColumns = id


-- | Pair two higher-kinded tables. This is primarily used to facilitate
-- generic deriving of higher-kinded tables with more than 1 field (it deals
-- with the @:*:@ case).
data HPair x y (f :: KContext) = HPair { hfst :: x f, hsnd :: y f }
  deriving stock (Generic)


-- | A HField type for indexing into HPair.
data HPairField x y a where
  HPairFst :: HField x a -> HPairField x y a
  HPairSnd :: HField y a -> HPairField x y a


instance (HTable x, HTable y) => HTable (HPair x y) where
  type HConstrainTable (HPair x y) c = (HConstrainTable x c, HConstrainTable y c)
  type HField (HPair x y) = HPairField x y

  hfield (HPair l r) = \case
    HPairFst i -> hfield l i
    HPairSnd i -> hfield r i

  htabulate f = HPair (htabulate (f . HPairFst)) (htabulate (f . HPairSnd))

  htraverse f (HPair x y) = HPair <$> htraverse f x <*> htraverse f y

  hdbtype = HPair hdbtype hdbtype


instance (Table f a, Table f b) => Table f (a, b) where
  type Columns (a, b) = HPair (Columns a) (Columns b)
  toColumns (a, b) = HPair (toColumns a) (toColumns b)
  fromColumns (HPair x y) = (fromColumns x, fromColumns y)


instance (Table f a, Table f b, Table f c) => Table f (a, b, c) where
  type Columns (a, b, c) = HPair (Columns a) (HPair (Columns b) (Columns c))
  toColumns (a, b, c) = HPair (toColumns a) $ HPair (toColumns b) (toColumns c)
  fromColumns (HPair x (HPair y z)) = (fromColumns x, fromColumns y, fromColumns z)


-- | A single-column higher-kinded table. This is primarily useful for
-- facilitating generic-deriving of higher kinded tables.
data HIdentity a context where
  HIdentity :: { unHIdentity :: f a } -> HIdentity a (Context f)


data HIdentityField x y where
  HIdentityField :: HIdentityField x x


instance DBType a => HTable (HIdentity a) where
  type HConstrainTable (HIdentity a) c = (c a)
  type HField (HIdentity a) = HIdentityField a

  hfield (HIdentity a) HIdentityField = a
  htabulate f = HIdentity $ f HIdentityField
  hdbtype = HIdentity typeInformation

  htraverse :: forall f g m. Applicative m => (forall x. f x -> m (g x)) -> HIdentity a (Context f) -> m (HIdentity a (Context g))
  htraverse f (HIdentity a) = HIdentity <$> f (a :: f a)


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
class (ExprFor expr haskell, Table Expr expr) => Serializable expr haskell | expr -> haskell where
  lit :: haskell -> expr

  rowParser :: forall f a. Applicative f
    => (forall x. Decoder x -> a -> Conversion (f x))
    -> Columns expr (Context (Const a))
    -> Conversion (f haskell)


-- | @ExprFor expr haskell@ witnesses that @expr@ is the "expression
-- representation" of the Haskell type @haskell@. You can think of this as the
-- type obtained if you were to quote @haskell@ constants into a query. 
--
-- This type class exists to provide "backwards" type inference for
-- 'Serializable'. While the functional dependency on 'Serializable' shows that
-- for any @expr@ there is exactly one @haskell@ type that is returned when the
-- expression is @select@ed, this type class is less restrictive, allowing for
-- their to be multiple expression types. Usually this is not the case, but for
-- @Maybe a@, we may allow expressions to be either @MaybeTable a'@ (where
-- @ExprFor a' a@), or just @Expr (Maybe a)@ (if @a@ is a single column).
class Table Expr expr => ExprFor expr haskell


instance {-# OVERLAPPABLE #-} (DBType b, a ~ Expr b)                              => ExprFor a                b
instance DBType a                                                                 => ExprFor (Expr (Maybe a)) (Maybe a)
instance (ExprFor a b, Table Expr a)                                              => ExprFor (MaybeTable a)   (Maybe b)
instance (a ~ ListTable x, Table Expr (ListTable x), ExprFor x b)                 => ExprFor a                [b]
instance (a ~ NonEmptyTable x, Table Expr (NonEmptyTable x), ExprFor x b)         => ExprFor a                (NonEmpty b)
instance (a ~ (a1, a2), ExprFor a1 b1, ExprFor a2 b2)                             => ExprFor a                (b1, b2)
instance (a ~ (a1, a2, a3), ExprFor a1 b1, ExprFor a2 b2, ExprFor a3 b3)          => ExprFor a                (b1, b2, b3)
instance (HTable t, a ~ t (Context Expr), identity ~ Context Identity)            => ExprFor a                (t identity)
instance (HigherKindedTable t, a ~ t Expr, identity ~ Identity)                   => ExprFor a                (t identity)


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (s ~ t, expr ~ Context Expr, identity ~ Context Identity, HTable t) => Serializable (s expr) (t identity) where
  rowParser :: forall f a. Applicative f
    => (forall x. Decoder x -> a -> Conversion (f x))
    -> Columns (s expr) (Context (Const a))
    -> Conversion (f (t identity))
  rowParser parseColumn columns = getCompose $ htraverse (fmap pure) $ htabulate f
    where
      f :: forall x. HField t x -> Compose Conversion f x
      f i = case (hfield columns i, hfield hdbtype i) of
        (Const a, databaseType) -> Compose $ parseColumn (decoder databaseType) a

  lit t =
    fromColumns $ htabulate \i ->
      case (hfield (hdbtype @t) i, hfield t i) of
        (databaseType, Identity x) -> monolit databaseType x


instance (s ~ t, expr ~ Expr, identity ~ Identity, HigherKindedTable t) => Serializable (s expr) (t identity) where
  lit = glit
  rowParser = growParser


instance (DBType a, a ~ b) => Serializable (Expr a) b where
  rowParser parseColumn (HIdentity (Const x)) = parseColumn (decoder typeInformation) x

  lit = Expr . Opaleye.CastExpr typeName . encode
    where
      DatabaseType{ encode, typeName } = typeInformation


instance (Serializable a1 b1, Serializable a2 b2) => Serializable (a1, a2) (b1, b2) where
  rowParser parseColumn (HPair x y) = liftA2 (liftA2 (,)) (rowParser @a1 parseColumn x) (rowParser @a2 parseColumn y)

  lit (a, b) = (lit a, lit b)


instance (Serializable a1 b1, Serializable a2 b2, Serializable a3 b3) => Serializable (a1, a2, a3) (b1, b2, b3) where
  rowParser parseColumn (HPair x (HPair y z)) = liftA3 (,,) <$> rowParser @a1 parseColumn x <*> rowParser @a2 parseColumn y <*> rowParser @a3 parseColumn z
  lit (a, b, c) = (lit a, lit b, lit c)


instance Serializable a b => Serializable (MaybeTable a) (Maybe b) where
  rowParser :: forall f t. Applicative f
    => (forall x. Decoder x -> t -> Conversion (f x))
    -> Columns (MaybeTable a) (Context (Const t))
    -> Conversion (f (Maybe b))
  rowParser parseColumn (HMaybeTable (HIdentity (Const tag)) columns) = do
    tags <- parseColumn (decoder typeInformation) tag
    rows <- rowParser @a (\x y -> Compose <$> parseColumn (acceptNull x) y) columns
    return $ liftA2 f tags (getCompose rows)
    where
      f :: Maybe Bool -> Maybe b -> Maybe b
      f (Just True)  (Just row) = Just row
      f (Just True)  Nothing    = error "TODO"
      f _            _          = Nothing

  lit = \case
    Nothing -> noTable
    Just x  -> pure $ lit x


type role Expr representational


instance (IsString a, DBType a) => IsString (Expr a) where
  fromString = monolit (typeInformation @a) . fromString


-- | @MaybeTable t@ is the table @t@, but as the result of an outer join. If
-- the outer join fails to match any rows, this is essentialy @Nothing@, and if
-- the outer join does match rows, this is like @Just@. Unfortunately, SQL
-- makes it impossible to distinguish whether or not an outer join matched any
-- rows based generally on the row contents - if you were to join a row
-- entirely of nulls, you can't distinguish if you matched an all null row, or
-- if the match failed.  For this reason @MaybeTable@ contains an extra field -
-- 'nullTag' - to track whether or not the outer join produced any rows.
data MaybeTable t where
  MaybeTable
    :: { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
         -- join matched any rows.
         nullTag :: Expr (Maybe Bool)
       , table :: t
       }
    -> MaybeTable t
  deriving stock Functor


instance Applicative MaybeTable where
  pure = MaybeTable (lit (Just True))
  MaybeTable t f <*> MaybeTable t' a = MaybeTable (liftOpNull (&&.) t t') (f a)


instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftOpNull (&&.) t t') b


data HMaybeTable g f = HMaybeTable
  { hnullTag :: HIdentity (Maybe Bool) f
  , htable :: g f
  }
  deriving stock Generic
  deriving anyclass HTable


instance Table Expr a => Table Expr (MaybeTable a) where
  type Columns (MaybeTable a) = HMaybeTable (Columns a)

  toColumns (MaybeTable x y) = HMaybeTable (HIdentity x) (toColumns y)
  fromColumns (HMaybeTable (HIdentity x) y) = MaybeTable x (fromColumns y)


type family HMaybe (context :: Type -> Type) (a :: Type) :: Type where
  HMaybe Identity a = Maybe a
  HMaybe Expr a     = MaybeTable a
  HMaybe f a        = HMaybeTable (Columns a) (Context f)


-- | Perform case analysis on a 'MaybeTable'. Like 'maybe'.
maybeTable
  :: Table Expr b
  => b -> (a -> b) -> MaybeTable a -> b
maybeTable def f MaybeTable{ nullTag, table } =
  ifThenElse_ (null_ (lit False) id nullTag) (f table) def


-- | The null table. Like 'Nothing'.
noTable :: forall a. Table Expr a => MaybeTable a
noTable = MaybeTable (lit Nothing) $ fromColumns $ htabulate f
  where
    f :: forall x. HField (Columns a) x -> Expr x
    f i =
      case hfield (hdbtype @(Columns a)) i of
        databaseType -> unsafeCoerceExpr (monolit (nullDatabaseType databaseType) (Nothing :: Maybe x))


instance (DBType a, expr ~ Expr) => Table expr (Expr a) where
  type Columns (Expr a) = HIdentity a
  toColumns = HIdentity
  fromColumns = unHIdentity


fromOpaleye :: forall a b. IsSqlType b
  => (a -> Opaleye.Column b)
  -> Decoder a
  -> DatabaseType a
fromOpaleye f decoder =
  DatabaseType
    { encode = \x -> case f x of Opaleye.Column e -> e
    , decoder
    , typeName = showSqlType (Proxy @b)
    }


-- | Corresponds to the @bool@ PostgreSQL type.
instance DBType Bool where
  typeInformation = fromOpaleye pgBool Decoder
    { decodeBytes = \case
        Just "t" -> pure True
        Just "f" -> pure False

    , decodeJSON = \case
        Data.Aeson.Bool x -> pure x
    }


-- | Corresponds to the @int4@ PostgreSQL type.
instance DBType Int32 where
  typeInformation = mapDatabaseType fromIntegral fromIntegral $ fromOpaleye pgInt4 Decoder
    { decodeBytes = \case
        Just bytes ->
          case parseOnly (signed decimal) bytes of
            Right ok -> pure ok

    , decodeJSON = \case
        Data.Aeson.Number n -> pure $ round n
    }


-- | Corresponds to the @int8@ PostgreSQL type.
instance DBType Int64 where
  typeInformation = fromOpaleye pgInt8 Decoder
    { decodeBytes = \case
        Just bytes ->
          case parseOnly (signed decimal) bytes of
            Right ok -> pure ok

    , decodeJSON = \case
        Data.Aeson.Number n -> pure $ round n
    }


instance DBType Float where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decoder = Decoder{ decodeBytes, decodeJSON }
    , typeName = "float4"
    }
    where
      decodeBytes = \case
        Just bytes ->
          case parseOnly (realToFrac <$> rational) bytes of
            Right ok -> pure ok

      decodeJSON = \case
        Data.Aeson.Number n -> pure $ realToFrac n


instance DBType UTCTime where
  typeInformation = fromOpaleye pgUTCTime $ parseDecoder parseUTCTime bytestringDecoder


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Text where
  typeInformation = fromOpaleye pgStrictText Decoder
    { decodeBytes = \case
        Just bytes -> pure $ decodeUtf8 bytes -- TODO Error checking

    , decodeJSON = \case
        Data.Aeson.String s -> pure s
    }


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Data.Text.Lazy.Text where
  typeInformation = mapDatabaseType Data.Text.Lazy.fromStrict Data.Text.Lazy.toStrict typeInformation


-- | Extends any @DBType@ with the value @null@. Note that you cannot "stack"
-- @Maybe@s, as SQL doesn't distinguish @Just Nothing@ from @Nothing@.
instance DBType a => DBType (Maybe a) where
  typeInformation = nullDatabaseType typeInformation


nullDatabaseType :: DatabaseType a -> DatabaseType (Maybe a)
nullDatabaseType DatabaseType{ encode, typeName, decoder } = DatabaseType
  { encode = maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
  , decoder = acceptNull decoder
  , typeName
  }


-- | Corresponds to the @json@ PostgreSQL type.
instance DBType Value where
  typeInformation = fromOpaleye pgValueJSON Decoder
    { decodeBytes = \case
        Just bytes ->
          case decode $ fromStrict bytes of
            Just ok -> pure ok

    , decodeJSON = pure
    }


instance DBType Data.ByteString.Lazy.ByteString where
  typeInformation = mapDatabaseType Data.ByteString.Lazy.fromStrict Data.ByteString.Lazy.toStrict typeInformation


instance DBType Data.ByteString.ByteString where
  typeInformation = fromOpaleye pgStrictByteString $ parseDecoder (toEither . unsafeDupablePerformIO . unescapeBytea) bytestringDecoder
    where
      toEither = maybe (Left "Could not decode ByteString") Right


instance DBType Scientific where
  typeInformation = fromOpaleye pgNumeric Decoder
    { decodeBytes = \case
        Just bytes ->
          case parseOnly rational bytes of
            Right ok -> pure ok

    , decodeJSON = \case
        Data.Aeson.Number n -> pure n
    }


instance DBType Double where
  typeInformation = fromOpaleye pgDouble Decoder{ decodeBytes, decodeJSON }
    where
      decodeBytes = \case
        Just bytes ->
          case parseOnly parser bytes of
            Right ok -> pure ok

      decodeJSON = \case
        Data.Aeson.Number n -> pure $ realToFrac n

      parser =   
            (string "NaN"       *> pure ( 0 / 0))
        <|> (string "Infinity"  *> pure ( 1 / 0))
        <|> (string "-Infinity" *> pure (-1 / 0))
        <|> double


instance DBType UUID where
  typeInformation = fromOpaleye pgUUID $ parseDecoder (toEither . UUID.fromASCIIBytes) bytestringDecoder
    where
      toEither = maybe (Left "Could not parse UUID") Right


instance DBType Day where
  typeInformation = fromOpaleye pgDay $ parseDecoder parseDay bytestringDecoder


instance DBType LocalTime where
  typeInformation = fromOpaleye pgLocalTime $ parseDecoder parseLocalTime bytestringDecoder


instance DBType ZonedTime where
  typeInformation = fromOpaleye pgZonedTime $ parseDecoder parseZonedTime bytestringDecoder


instance DBType TimeOfDay where
  typeInformation = fromOpaleye pgTimeOfDay $ parseDecoder parseTimeOfDay bytestringDecoder


instance DBType (CI Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }


instance DBType (CI Data.Text.Lazy.Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }


instance DBType a => DBType [a] where
  typeInformation = liftDatabaseType typeInformation


instance DBType a => DBType (NonEmpty a) where
  typeInformation = liftDatabaseType typeInformation


case_ :: forall a. Table Expr a => [ ( Expr Bool, a ) ] -> a -> a
case_ alts def =
  fromColumns $ htabulate @(Columns a) \x -> fromPrimExpr $
    Opaleye.CaseExpr
        [ ( toPrimExpr bool, toPrimExpr $ hfield (toColumns alt) x ) | ( bool, alt ) <- alts ]
        ( toPrimExpr $ hfield (toColumns def) x )


retype :: forall b a. Expr a -> Expr b
retype = fromPrimExpr . toPrimExpr


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


-- | The 'DBType' instance for 'ReadShow' allows you to serialize a type using
-- Haskell's 'Read' and 'Show' instances:
--
-- @
-- data Color = Red | Green | Blue
--   deriving (Read, Show)
--   deriving DBType via ReadShow Color
-- @
instance (Read a, Show a, Typeable a) => DBType (ReadShow a) where
  typeInformation =
    parseDatabaseType (fmap ReadShow . readEither . Text.unpack) (Text.pack . show . fromReadShow) typeInformation


mapTable
  :: (Congruent s t, Table f s, Table g t)
  => (forall x. f x -> g x) -> s -> t
mapTable f = fromColumns . runIdentity . htraverse (pure . f) . toColumns


zipTablesWithM
  :: forall x y z f g h m
   . (Congruent x y, Columns y ~ Columns z, Table f x, Table g y, Table h z, Applicative m)
  => (forall a. f a -> g a -> m (h a)) -> x -> y -> m z
zipTablesWithM f (toColumns -> x) (toColumns -> y) =
  fmap fromColumns $
    htraverse getCompose $
      htabulate @_ @(Compose m h) $
        Compose . liftA2 f (hfield x) (hfield y)


traverseTable
  :: (Congruent x y, Table f x, Table g y, Applicative m)
  => (forall a. f a -> m (g a)) -> x -> m y
traverseTable f = fmap fromColumns . htraverse f . toColumns


binExpr :: Opaleye.BinOp -> Expr a -> Expr a -> Expr b
binExpr op ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr op a b )


column :: String -> Expr a
column columnName =
  Expr ( Opaleye.BaseTableAttrExpr columnName )


traversePrimExpr
  :: Applicative f
  => ( Opaleye.PrimExpr -> f Opaleye.PrimExpr ) -> Expr a -> f ( Expr a )
traversePrimExpr f =
  fmap fromPrimExpr . f . toPrimExpr


instance DBEq Int32


instance DBEq Int64


instance DBEq Text


instance DBEq Bool


instance DBEq a => DBEq ( Maybe a ) where
  eqExprs a b =
    null_ ( isNull b ) ( \a' -> null_ ( lit False ) ( eqExprs a' ) b ) a


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
select :: (Serializable row haskell, MonadIO m) => Connection -> Query row -> m [haskell]
select conn query =
  maybe
    ( return [] )
    ( liftIO . Database.PostgreSQL.Simple.queryWith_ ( queryParser query ) conn . fromString )
    ( selectQuery query )


queryParser
  :: Serializable sql haskell
  => Query sql
  -> Database.PostgreSQL.Simple.RowParser haskell
queryParser ( Query q ) =
  Opaleye.prepareRowParser
    queryRunner
    ( case Opaleye.runSimpleQueryArrStart q () of
        ( b, _, _ ) ->
          b
    )


queryRunner
  :: forall row haskell
   . Serializable row haskell
  => Opaleye.FromFields row haskell
queryRunner = Opaleye.QueryRunner (void unpackspec) (const parser) (const 1)
  where
  parser :: RowParser haskell
  parser = do
    unparsed <- htraverse (\_ -> getField) $ htabulate \_ -> Const ()
    RP $ lift $ lift $ runIdentity <$> rowParser @row useFieldParser unparsed

  getField :: RowParser (Const (Maybe ByteString) x)
  getField = fieldWith \_ -> pure . Const

  useFieldParser :: Decoder x -> Maybe ByteString -> Conversion (Identity x)
  useFieldParser = fmap (fmap pure) <$> decodeBytes


unpackspec :: Table Expr row => Opaleye.Unpackspec row row
unpackspec =
  Opaleye.Unpackspec $ Opaleye.PackMap \f ->
    fmap fromColumns . htraverse (traversePrimExpr f) . toColumns


-- | Run an @INSERT@ statement
--
-- >>> :{
-- insert c Insert
--   { into = authorSchema
--   , rows = [ lit Author{ authorName = "Gabriel Gonzales", authorId = AuthorId 4, authorUrl = Just "https://haskellforall.com" } ]
--   , onConflict = Abort
--   , returning = NumberOfRowsAffected
--   }
-- :}
-- 1
insert :: MonadIO m => Connection -> Insert result -> m result
insert connection Insert{ into, rows, onConflict, returning } =
  liftIO
    ( Opaleye.runInsert_
        connection
        ( toOpaleyeInsert into rows returning )
    )

  where

    toOpaleyeInsert
      :: forall schema result value
       . Selects schema value
      => TableSchema schema
      -> [ value ]
      -> Returning schema result
      -> Opaleye.Insert result
    toOpaleyeInsert into_ iRows returning_ =
      Opaleye.Insert
        { iTable = ddlTable into_ ( writer into_ )
        , iRows
        , iReturning = opaleyeReturning returning_
        , iOnConflict
        }

      where

        iOnConflict :: Maybe Opaleye.OnConflict
        iOnConflict =
          case onConflict of
            DoNothing ->
              Just Opaleye.DoNothing

            Abort ->
              Nothing


writer
  :: forall value schema
   . Selects schema value
  => TableSchema schema -> Opaleye.Writer value schema
writer into_ =
  let
    go
      :: forall f list
       . ( Functor list, Applicative f )
      => ( ( list Opaleye.PrimExpr, String ) -> f () )
      -> list value
      -> f ()
    go f xs =
      void $
        htraverse @(Columns schema) @_ @Expr getCompose $
          htabulate @(Columns schema) @(Compose f Expr) \i ->
            case hfield (toColumns (tableColumns into_)) i of
              ColumnSchema{ columnName } ->
                Compose $
                  column columnName <$
                  f ( toPrimExpr . flip hfield i . toColumns <$> xs
                    , columnName
                    )

  in
  Opaleye.Writer ( Opaleye.PackMap go )


opaleyeReturning :: Returning schema result -> Opaleye.Returning schema result
opaleyeReturning returning =
  case returning of
    NumberOfRowsAffected ->
      Opaleye.Count

    Projection f ->
      Opaleye.ReturningExplicit
        queryRunner
        ( f . mapTable ( column . columnName ) )


ddlTable :: TableSchema schema -> Opaleye.Writer value schema -> Opaleye.Table value schema
ddlTable schema writer_ =
  toOpaleyeTable schema writer_ $ Opaleye.View (tableColumns schema)


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: Type -> Type where
  Insert
    :: Selects schema value
    => { into :: TableSchema schema
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
    :: ( Selects schema row, Serializable projection a )
    => (row -> projection)
    -> Returning schema [a]


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
data OnConflict
  = Abort     -- ^ @ON CONFLICT ABORT@
  | DoNothing -- ^ @ON CONFLICT DO NOTHING@


selectQuery :: forall a . Table Expr a => Query a -> Maybe String
selectQuery (Query opaleye) = showSqlForPostgresExplicit
  where
    showSqlForPostgresExplicit =
      case Opaleye.runQueryArrUnpack unpackspec opaleye of
        (x, y, z) -> Opaleye.formatAndShowSQL (x , Rel8.Optimize.optimize (Opaleye.optimize y) , z)


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
delete :: MonadIO m => Connection -> Delete from returning -> m returning
delete c Delete{ from = deleteFrom, deleteWhere, returning } =
  liftIO $ Opaleye.runDelete_ c $ go deleteFrom deleteWhere returning

  where

    go
      :: forall schema r row
       . Selects schema row
      => TableSchema schema
      -> (row -> Expr Bool)
      -> Returning schema r
      -> Opaleye.Delete r
    go schema deleteWhere_ returning_ =
      Opaleye.Delete
        { dTable = ddlTable schema $ Opaleye.Writer $ pure ()
        , dWhere =
            Opaleye.Column
              . toPrimExpr
              . deleteWhere_
              . mapTable (column . columnName)
        , dReturning = opaleyeReturning returning_
        }


-- | The constituent parts of a @DELETE@ statement.
data Delete from return where
  Delete
    :: Selects from row
    => { from :: TableSchema from
         -- ^ Which table to delete from.
       , deleteWhere :: row -> Expr Bool
         -- ^ Which rows should be selected for deletion.
       , returning :: Returning from return
         -- ^ What to return from the @DELETE@ statement.
       }
    -> Delete from return


-- | Run an @UPDATE@ statement.
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
--
-- >>> :{
-- update c Update
--   { target = projectSchema
--   , set = \p -> p { projectName = "Rel8!" }
--   , updateWhere = \p -> projectName p ==. lit "rel8"
--   , returning = NumberOfRowsAffected
--   }
-- :}
-- 1
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
-- Project {projectAuthorId = 1, projectName = "Rel8!"}
update :: MonadIO m => Connection -> Update target returning -> m returning
update connection Update{ target, set, updateWhere, returning } =
  liftIO $ Opaleye.runUpdate_ connection (go target set updateWhere returning)

  where

    go
      :: forall returning target row
       . Selects target row
      => TableSchema target
      -> (row -> row)
      -> (row -> Expr Bool)
      -> Returning target returning
      -> Opaleye.Update returning
    go target_ set_ updateWhere_ returning_ =
      Opaleye.Update
        { uTable =
            ddlTable target_ (writer target_)

        , uReturning =
            opaleyeReturning returning_

        , uWhere =
            Opaleye.Column
              . toPrimExpr
              . updateWhere_
              . mapTable (column . columnName)

        , uUpdateWith =
            set_ . mapTable (column . columnName)
        }


-- | The constituent parts of an @UPDATE@ statement.
data Update target returning where
  Update
    :: Selects target row
    => { target :: TableSchema target
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
exists :: Query a -> Query (Expr Bool)
exists = fmap (maybeTable (lit False) (const (lit True))) .
  optional . mapOpaleye Opaleye.restrictExists


-- | Select each row from a table definition. This is equivalent to @FROM
-- table@.
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
each :: Selects schema row => TableSchema schema -> Query row
each = liftOpaleye . Opaleye.selectTableExplicit unpackspec . f
  where
    f :: forall schema row.  Selects schema row => TableSchema schema -> Opaleye.Table () row
    f schema = toOpaleyeTable schema noWriter view
      where
        noWriter :: Opaleye.Writer () row
        noWriter = Opaleye.Writer $ Opaleye.PackMap \_ _ -> pure ()

        view :: Opaleye.View row
        view = Opaleye.View $ mapTable (column . columnName) (tableColumns schema)


-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, a null 'MaybeTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
optional :: Query a -> Query (MaybeTable a)
optional = mapOpaleye $ Opaleye.laterally (Opaleye.QueryArr . go)
  where
    go query (i, left, tag) = (MaybeTable t' a, join, Opaleye.next tag')
      where
        (MaybeTable t a, right, tag') = Opaleye.runSimpleQueryArr (pure <$> query) (i, tag)
        (t', bindings) = Opaleye.run $ Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "maybe" tag') t
        join = Opaleye.Join Opaleye.LeftJoin (toPrimExpr $ lit True) [] bindings left right


-- | Combine the results of two queries of the same type, collapsing
-- duplicates.  @union a b@ is the same as the SQL statement @x UNION b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `union` values [lit True]
-- [False,True]
union :: Table Expr a => Query a -> Query a -> Query a
union l r = liftOpaleye $ Opaleye.unionExplicit binaryspec (toOpaleye l) (toOpaleye r)
  where
    binaryspec :: Table Expr a => Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
        zipTablesWithM (\x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


-- | Combine the results of two queries of the same type, retaining duplicates.
-- @unionAll a b@ is the same as the SQL statement @x UNION ALL b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `unionAll` values [lit True]
-- [True,True,False,True]
unionAll :: Table Expr a => Query a -> Query a -> Query a
unionAll l r = liftOpaleye $ Opaleye.unionAllExplicit binaryspec (toOpaleye l) (toOpaleye r)
  where
    binaryspec :: Table Expr a => Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
        zipTablesWithM (\x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


-- | Find the intersection of two queries, collapsing duplicates.  @intersect a
-- b@ is the same as the SQL statement @x INTERSECT b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `intersect` values [lit True]
-- [True]
intersect :: Table Expr a => Query a -> Query a -> Query a
intersect l r = liftOpaleye $ Opaleye.intersectExplicit binaryspec (toOpaleye l) (toOpaleye r)
  where
    binaryspec :: Table Expr a => Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
        zipTablesWithM (\x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


-- | Find the intersection of two queries, retaining duplicates.  @intersectAll
-- a b@ is the same as the SQL statement @x INTERSECT ALL b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `intersectAll` values [lit True, lit True]
-- [True,True]
intersectAll :: Table Expr a => Query a -> Query a -> Query a
intersectAll l r = liftOpaleye $ Opaleye.intersectAllExplicit binaryspec (toOpaleye l) (toOpaleye r)
  where
    binaryspec :: Table Expr a => Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
        zipTablesWithM (\x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


-- | Find the difference of two queries, collapsing duplicates @except a b@ is
-- the same as the SQL statement @x INTERSECT b@.
--
-- >>> select c $ values [lit True, lit False, lit False] `except` values [lit True]
-- [False]
except :: Table Expr a => Query a -> Query a -> Query a
except l r = liftOpaleye $ Opaleye.exceptExplicit binaryspec (toOpaleye l) (toOpaleye r)
  where
    binaryspec :: Table Expr a => Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
        zipTablesWithM (\x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


-- | Find the difference of two queries, retaining duplicates.  @exceptAll a b@
-- is the same as the SQL statement @x EXCEPT ALL b@.
--
-- >>> select c $ values [lit True, lit False, lit False] `exceptAll` values [lit True]
-- [False,False]
exceptAll :: Table Expr a => Query a -> Query a -> Query a
exceptAll l r = liftOpaleye $ Opaleye.exceptAllExplicit binaryspec (toOpaleye l) (toOpaleye r)
  where
    binaryspec :: Table Expr a => Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
        zipTablesWithM (\x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


-- | Select all distinct rows from a query, removing duplicates.  @distinct q@
-- is equivalent to the SQL statement @SELECT DISTINCT q@.
--
-- >>> select c $ distinct $ values [ lit True, lit True, lit False ]
-- [False,True]
distinct :: Table Expr a => Query a -> Query a
distinct = mapOpaleye (Opaleye.distinctExplicit distinctspec)
  where
    distinctspec :: Table Expr a => Opaleye.Distinctspec a a
    distinctspec =
      Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f ->
        traverseTable (\x -> fromPrimExpr <$> f (Nothing, toPrimExpr x))


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
where_ x =
  liftOpaleye $ Opaleye.QueryArr \((), left, t) ->
    ((), Opaleye.restrict (toPrimExpr x) left, t)


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
catMaybeTable :: MaybeTable a -> Query a
catMaybeTable MaybeTable{ nullTag, table } = do
  where_ $ not_ $ isNull nullTag
  return table


-- | Construct a query that returns the given input list of rows. This is like
-- folding a list of 'return' statements under 'union', but uses the SQL
-- @VALUES@ expression for efficiency.
--
-- Typically @values@ will be used with 'lit':
--
-- >>> mapM_ Data.Text.IO.putStrLn =<< select c (values [ lit "Hello", lit "World!" ])
-- Hello
-- World!
values :: forall expr f. (Table Expr expr, Foldable f) => f expr -> Query expr
values = liftOpaleye . Opaleye.valuesExplicit valuesspec . toList
  where
    valuesspec = Opaleye.ValuesspecSafe packmap unpackspec
      where
        packmap :: Opaleye.PackMap Opaleye.PrimExpr Opaleye.PrimExpr () expr
        packmap = Opaleye.PackMap \f () ->
          fmap fromColumns $
            htraverse (traversePrimExpr f) $
              htabulate @(Columns expr) @Expr \i ->
                case hfield (hdbtype @(Columns expr)) i of
                  databaseType -> fromPrimExpr $ nullExpr databaseType
            where
              nullExpr :: DatabaseType a -> Opaleye.PrimExpr
              nullExpr DatabaseType{ typeName } = Opaleye.CastExpr typeName (Opaleye.ConstExpr Opaleye.NullLit)


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


-- | Any @Expr@s can be compared for equality as long as the underlying
-- database type supports equality comparisons.
instance DBEq a => EqTable (Expr a) where
  (==.) = eqExprs


-- | The schema for a column in a table. To construct values of this type,
-- enable the @OverloadedStrings@ language extension and write literal Haskell
-- strings:
--
-- >>> :{
-- -- You would usually just inline this in your TableSchema definition.
-- authorColumns :: Author ColumnSchema
-- authorColumns = Author
--   { authorName = "name" 
--   , authorId = "author_id" 
--   , authorUrl = "url" 
--   }
-- :}
--
-- If you want to programatically create @ColumnSchema@'s, you can use 'Data.String.fromString':
--
-- >>> fromString ("hello" ++ "_" ++ "world") :: ColumnSchema Bool
newtype ColumnSchema (a :: Type) =
  ColumnSchema { columnName :: String }


-- | You can construct @ColumnSchema@ values by using @\{\-\# LANGUAGE
-- OverloadedStrings #-\}@ and writing literal strings in your source code.
instance IsString (ColumnSchema a) where
  fromString = ColumnSchema


instance (DBType a, f ~ ColumnSchema) => Table f (ColumnSchema a) where
  type Columns (ColumnSchema a) = HIdentity a
  toColumns = HIdentity
  fromColumns = unHIdentity


toOpaleyeTable
  :: TableSchema schema
  -> Opaleye.Writer write view
  -> Opaleye.View view
  -> Opaleye.Table write view
toOpaleyeTable TableSchema{ tableName, tableSchema } writer_ view =
  maybe withoutSchema withSchema tableSchema
  where
    tableFields = Opaleye.TableFields writer_ view

    withoutSchema = Opaleye.Table tableName tableFields
    withSchema s = Opaleye.TableWithSchema s tableName tableFields


-- | Convert a query to a 'String' containing the query as a @SELECT@
-- statement.
showQuery :: Table Expr a => Query a -> String
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
listAgg :: Table Expr exprs => exprs -> Aggregate (ListTable exprs)
listAgg = fmap ListTable . traverseTable (fmap ComposeInner . go)
  where
    go :: Expr a -> Aggregate (Expr [a])
    go (pgtoJSONB -> Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll (Opaleye.AggrOther "jsonb_agg") a []


pgtoJSONB :: Expr a -> Expr Value
pgtoJSONB = function "to_jsonb"


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Table Expr exprs => exprs -> Aggregate (NonEmptyTable exprs)
nonEmptyAgg = fmap NonEmptyTable . traverseTable (fmap ComposeInner . go)
  where
    go :: Expr a -> Aggregate (Expr (NonEmpty a))
    go (pgtoJSONB -> Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll (Opaleye.AggrOther "jsonb_agg") a []


-- | The class of 'DBType's that support the @max@ aggregation function.
--
-- If you have a custom type that you know supports @max@, you can use
-- @DeriveAnyClass@ to derive a default implementation that calls @max@.
class DBMax a where
  -- | Produce an aggregation for @Expr a@ using the @max@ function.
  max :: Expr a -> Aggregate (Expr a)
  max (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrMax a []


instance DBMax Int64
instance DBMax Double
instance DBMax Int32
instance DBMax Scientific
instance DBMax Float
instance DBMax Text


instance DBMax a => DBMax (Maybe a) where
  max expr = retype <$> max (retype @a expr)


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: forall a. Table Expr a => Query (Aggregate a) -> Query a
aggregate = mapOpaleye $ Opaleye.aggregate aggregator
  where
    aggregator :: Opaleye.Aggregator (Aggregate a) a
    aggregator = Opaleye.Aggregator $ Opaleye.PackMap \f (Aggregate x) ->
      fromColumns <$> htraverse (g f) (toColumns x)

    g :: forall m x. Applicative m => ((Maybe (Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct), Opaleye.PrimExpr) -> m Opaleye.PrimExpr) -> Expr x -> m (Expr x)
    g f (Expr x) | hasAggrExpr x = Expr <$> traverseAggrExpr f' x
                 | otherwise     = Expr <$> f (Nothing, x)
      where f' (a, b, c, d) = f (Just (a, b, c), d)


hasAggrExpr :: Opaleye.PrimExpr -> Bool
hasAggrExpr = getAny . getConst . traverseAggrExpr (\_ -> Const (Any True))


traverseAggrExpr :: Applicative f => ((Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct, Opaleye.PrimExpr) -> f Opaleye.PrimExpr) -> Opaleye.PrimExpr -> f Opaleye.PrimExpr
traverseAggrExpr f = \case
  Opaleye.AggrExpr a b c d ->
    f (b, d, a, c)

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

  Opaleye.AttrExpr attr ->
    pure $ Opaleye.AttrExpr attr

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


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'many' or 'listAgg'.
newtype ListTable a = ListTable (Columns a (Context (ComposeInner (Context Expr) [])))


instance (f ~ Expr, Table f a) => Table f (ListTable a) where
  type Columns (ListTable a) = HComposeTable [] (Columns a)

  toColumns (ListTable a) = HComposeTable a
  fromColumns (HComposeTable a) = ListTable a


instance Serializable a b => Serializable (ListTable a) [b] where
  rowParser parseColumn xs = fmap getZipList . getCompose <$> rowParser @a (\x y -> Compose <$> parseColumn (listOf x) y) (f xs)
    where
    listOf :: Decoder x -> Decoder (ZipList x)
    listOf Decoder{ decodeJSON } = Decoder
      { decodeJSON = go
      , decodeBytes = \case
          Nothing    -> error "TODO"
          Just bytes ->
            case decodeStrict bytes of
              Just x -> go x
      }
      where
        go (Data.Aeson.Array xs) = traverse decodeJSON (ZipList (toList xs))

    f :: HComposeTable [] (Columns a) (Context (Const t)) -> Columns a (Context (Const t))
    f (HComposeTable ys) = hmap g ys
      where
        g :: ComposeInner (Context (Const t)) [] x -> Const t x
        g (ComposeInner (Const x)) = Const x


  lit (map (lit @a) -> xs) = ListTable $ htabulate $ \field ->
    case hfield hdbtype field of
      databaseType -> ComposeInner $ listOfExprs databaseType $
        map (\x -> hfield (toColumns x) field) xs
    where
      listOfExprs :: DatabaseType x -> [Expr x] -> Expr [x]
      listOfExprs databaseType as = fromPrimExpr $
        Opaleye.CastExpr array $
        Opaleye.ArrayExpr (map toPrimExpr as)
        where
          array = typeName (liftDatabaseType @[] databaseType)


instance Table Expr a => Semigroup (ListTable a) where
  ListTable a <> ListTable b =
    ListTable (hzipWith (zipComposeInnerWith (binaryOperator "||")) a b)


instance Table Expr a => Monoid (ListTable a) where
  mempty = ListTable $ htabulate $ \field ->
    case hfield hdbtype field of
      databaseType -> ComposeInner $ monolit (liftDatabaseType databaseType) []


type family HList (context :: Type -> Type) (a :: Type) :: Type where
  HList Identity a = [a]
  HList Expr a     = ListTable a
  HList f a        = HComposeTable [] (Columns a) (Context f)


-- | Aggregate a 'Query' into a 'ListTable'. If the supplied query returns 0
-- rows, this function will produce a 'Query' that returns one row containing
-- the empty @ListTable@. If the supplied @Query@ does return rows, @many@ will
-- return exactly one row, with a @ListTable@ collecting all returned rows.
-- 
-- @many@ is analogous to 'Control.Applicative.many' from
-- @Control.Applicative@.
many :: Table Expr exprs => Query exprs -> Query (ListTable exprs)
many = fmap (maybeTable mempty id) . optional . aggregate . fmap listAgg


-- | A @NonEmptyTable@ value contains one or more instances of @a@. You
-- construct @NonEmptyTable@s with 'some' or 'nonEmptyAgg'.
newtype NonEmptyTable a = NonEmptyTable (Columns a (Context (ComposeInner (Context Expr) NonEmpty)))


instance (f ~ Expr, Table f a) => Table f (NonEmptyTable a) where
  type Columns (NonEmptyTable a) = HComposeTable NonEmpty (Columns a)

  toColumns (NonEmptyTable a) = HComposeTable a
  fromColumns (HComposeTable a) = NonEmptyTable a


instance Serializable a b => Serializable (NonEmptyTable a) (NonEmpty b) where
  rowParser parseColumn xs = fmap (NonEmpty.fromList . getZipList) . getCompose <$> rowParser @a (\x y -> Compose <$> parseColumn (listOf x) y) (f xs)
    where
    listOf :: Decoder x -> Decoder (ZipList x)
    listOf Decoder{ decodeJSON } = Decoder
      { decodeJSON = go
      , decodeBytes = \case
          Nothing    -> error "TODO"
          Just bytes ->
            case decodeStrict bytes of
              Just x -> go x
      }
      where
        go (Data.Aeson.Array xs) = traverse decodeJSON (ZipList (toList xs))

    f :: HComposeTable NonEmpty (Columns a) (Context (Const t)) -> Columns a (Context (Const t))
    f (HComposeTable ys) = hmap g ys
      where
        g :: ComposeInner (Context (Const t)) NonEmpty x -> Const t x
        g (ComposeInner (Const x)) = Const x

  lit (fmap (lit @a) -> xs) = NonEmptyTable $ htabulate $ \field ->
    case hfield hdbtype field of
      databaseType -> ComposeInner $ nonEmptyOf databaseType $
        fmap (\x -> hfield (toColumns x) field) xs
    where
      nonEmptyOf :: DatabaseType x -> NonEmpty (Expr x) -> Expr (NonEmpty x)
      nonEmptyOf databaseType as = fromPrimExpr $
        Opaleye.CastExpr array $
        Opaleye.ArrayExpr (map toPrimExpr (toList as))
        where
          array = typeName (liftDatabaseType @NonEmpty databaseType)


instance Table Expr a => Semigroup (NonEmptyTable a) where
  NonEmptyTable a <> NonEmptyTable b =
    NonEmptyTable (hzipWith (zipComposeInnerWith (binaryOperator "||")) a b)


type family HNonEmpty (context :: Type -> Type) (a :: Type) :: Type where
  HNonEmpty Identity a = NonEmpty a
  HNonEmpty Expr a     = NonEmptyTable a
  HNonEmpty f a        = HComposeTable NonEmpty (Columns a) (Context f)


-- | Aggregate a 'Query' into a 'NonEmptyTable'. If the supplied query returns
-- 0 rows, this function will produce a 'Query' that is empty - that is, will
-- produce zero @NonEmptyTable@s. If the supplied @Query@ does return rows,
-- @some@ will return exactly one row, with a @NonEmptyTable@ collecting all
-- returned rows.
--
-- @some@ is analogous to 'Control.Applicative.some' from
-- @Control.Applicative@.
some :: Table Expr exprs => Query exprs -> Query (NonEmptyTable exprs)
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
asc :: DBType a => Order (Expr a)
asc = Order $ Opaleye.Order (getConst . htraverse f . toColumns)
  where
    f :: forall x. Expr x -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (Expr x)
    f (Expr primExpr) = Const [(orderOp, primExpr)]

    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpAsc
      , orderNulls = Opaleye.NullsLast
      }


-- | Sort a column in descending order.
--
-- >>> select c $ orderBy desc $ values [ lit x | x <- [1..5 :: Int32] ]
-- [5,4,3,2,1]
desc :: DBType a => Order (Expr a)
desc = Order $ Opaleye.Order (getConst . htraverse f . toColumns)
  where
    f :: forall x. Expr x -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (Expr x)
    f (Expr primExpr) = Const [(orderOp, primExpr)]

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
nullsFirst :: Order (Expr (Maybe a)) -> Order (Expr (Maybe a))
nullsFirst (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsFirst }


-- | Transform an ordering so that @null@ values appear first. This corresponds
-- to @NULLS LAST@ in SQL.
--
-- >>> select c $ orderBy (nullsLast desc) $ values $ [ nullExpr, nullExpr ] <> [ lit (Just x) | x <- [1..5 :: Int32] ]
-- [Just 5,Just 4,Just 3,Just 2,Just 1,Nothing,Nothing]
nullsLast :: Order (Expr (Maybe a)) -> Order (Expr (Maybe a))
nullsLast (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsLast }


-- | Order the rows returned by a 'Query' according to a particular 'Order'.
--
-- For an example of using this, see the documentation for 'Order'.
orderBy :: Order a -> Query a -> Query a
orderBy (Order o) = liftOpaleye . Opaleye.laterally (Opaleye.orderBy o) . toOpaleye


-- | We say that two 'Table's are congruent if they have the same set of
-- columns. This is primarily useful for operations like @SELECT FROM@, where
-- we have a @Table@ of @ColumnSchema@s, and need to select them to a
-- corresponding @Table@ of @Expr@s.
class (Columns a ~ Columns b) => Congruent a b
instance (Columns a ~ Columns b) => Congruent a b


-- | We say that @Table a@ "selects" @Table b@ if @a@ and @b@ are 'Congruent',
-- @a@ contains 'ColumnSchema's and @b@ contains 'Expr's.
class (Congruent schema exprs, Table Expr exprs, Table ColumnSchema schema) => Selects schema exprs | schema -> exprs
instance (HigherKindedTable t, s ~ t, columnSchema ~ ColumnSchema, expr ~ Expr) => Selects (s columnSchema) (t expr)


-- Compose things
class c (f a) => ComposeConstraint c f a
instance c (f a) => ComposeConstraint c f a


data ComposeInner context g a where
  ComposeInner :: { getComposeInner :: f (g a) } -> ComposeInner (Context f) g a


traverseComposeInner :: forall f g t m a. Applicative m
  => (forall x. f x -> m (g x))
  -> ComposeInner (Context f) t a -> m (ComposeInner (Context g) t a)
traverseComposeInner f (ComposeInner a) =
  ComposeInner <$> f a


zipComposeInnerWith :: forall f g h t a. ()
  => (forall x. f x -> g x -> h x)
  -> ComposeInner (Context f) t a -> ComposeInner (Context g) t a -> ComposeInner (Context h) t a
zipComposeInnerWith f (ComposeInner a) (ComposeInner b) =
  ComposeInner $ f a b


data HComposeField f t a where
  HComposeField :: HField t a -> HComposeField f t (f a)


newtype HComposeTable g t (f :: KContext) = HComposeTable (t (Context (ComposeInner f g)))


instance (HTable t, DBFunctor f) => HTable (HComposeTable f t) where
  type HField (HComposeTable f t) = HComposeField f t
  type HConstrainTable (HComposeTable f t) c = HConstrainTable t (ComposeConstraint c f)

  hfield (HComposeTable columns) (HComposeField field) =
    getComposeInner (hfield columns field)

  htabulate f = HComposeTable (htabulate (ComposeInner . f . HComposeField))

  htraverse f (HComposeTable t) = HComposeTable <$> htraverse (traverseComposeInner f) t

  hdbtype :: HComposeTable f t (Context DatabaseType)
  hdbtype = HComposeTable $ hmap (ComposeInner . liftDatabaseType) hdbtype


class DBFunctor f where
  liftDatabaseType :: DatabaseType a -> DatabaseType (f a)


instance DBFunctor [] where
  liftDatabaseType DatabaseType{ encode, typeName } = DatabaseType
    { encode = Opaleye.FunExpr "to_jsonb" . pure . Opaleye.FunExpr "array_to_json" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map encode
    , typeName = "jsonb"
    }


instance DBFunctor NonEmpty where
  liftDatabaseType = parseDatabaseType nonEmptyEither toList . liftDatabaseType
    where
      nonEmptyEither =
        maybe (Left "DBType.NonEmpty.decode: empty list") Right . nonEmpty


class DBEq a => DBOrd (a :: Type) where
  -- | The PostgreSQL @<@ operator.
  (<.) :: Expr a -> Expr a -> Expr Bool
  a <. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..< exprToColumn b)

  -- | The PostgreSQL @<=@ operator.
  (<=.) :: Expr a -> Expr a -> Expr Bool
  a <=. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..<= exprToColumn b)

  -- | The PostgreSQL @>@ operator.
  (>.) :: Expr a -> Expr a -> Expr Bool
  a >. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..> exprToColumn b)

  -- | The PostgreSQL @>@ operator.
  (>=.) :: Expr a -> Expr a -> Expr Bool
  a >=. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..>= exprToColumn b)


instance DBOrd Bool where
instance DBOrd Int32 where
instance DBOrd Int64 where
instance DBOrd Text where

