{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8
  ( -- $intro

    -- * Defining Tables
    C
  , HasDefault(..)
  , Nullable(..)
  , BaseTable(..)

    -- * Tables
  , Table(..)
  , leftJoin, inlineLeftJoin
  , MaybeTable(..)
  , Col(..)

    -- * Expressions
  , Expr, coerceExpr, dbShow

    -- ** Equality
  , DBEq, (==.), (?=.), in_, ilike

    -- ** Ordering
  , DBOrd, (>.), (>=.), (<.), (<=.)

    -- ** Numeric Operators
  , DBNum(..)

    -- ** Boolean-valued expressions
  , (&&.), (||.), not

    -- ** Literals
  , DBType(..), lit, dbNow
  , TypeInfo(..), showableDbType, compositeDBType

    -- ** Null
  , toNullable , (?), isNull, nullable

    -- * Aggregation
  , AggregateTable(..), aggregate
  , count, groupBy, DBSum(..), countStar, DBMin(..), DBMax(..), avg
  , boolAnd, boolOr, stringAgg, arrayAgg, countDistinct
  , countRows, Aggregate

    -- * Querying Tables
  , O.Query, O.QueryArr
  , select
  , QueryResult
  , label

    -- ** Filtering
  , where_
  , filterQuery
  , distinct
  , Predicate

    -- ** Offset and limit
  , O.limit
  , O.offset

    -- ** Ordering
  , asc, desc, orderNulls, O.orderBy, OrderNulls(..)

    -- * Modifying tables
  , insert, insert1Returning, insertReturning
  , update, updateReturning
  , delete
  , Default(..), Insert

    -- * Re-exported symbols
  , Connection, Stream, Of, Generic

    -- * Unsafe routines
  , unsafeCoerceExpr
  , unsafeCastExpr
  , dbFunction
  , nullaryFunction
  , dbBinOp
  ) where

import Rel8.DBType
import Rel8.Internal.Operators
import Rel8.Internal.Aggregate
import Rel8.Internal.BaseTable
import Rel8.Internal.Expr
import Rel8.Internal.Table
import Rel8.Internal.Types
import Rel8.Internal.Order

import Control.Applicative (liftA2)
import Control.Category ((.), id)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Coerce (Coercible)
import Data.Int (Int16, Int32, Int64)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Profunctor (lmap)
import Data.Profunctor.Product ((***!))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime, LocalTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Generics.OneLiner
       (ADTRecord, Constraints, For(..), gfoldMap)
import qualified Opaleye.Aggregate as O
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.Distinct as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Join as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.PrimQuery as PrimQuery
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.RunQuery as O
import qualified Opaleye.Internal.Unpackspec as O
import qualified Opaleye.Join as O
import Opaleye.Label (label)
import qualified Opaleye.Manipulation as O
import qualified Opaleye.Operators as O
import qualified Opaleye.Order as O
import qualified Opaleye.RunQuery as O
import Prelude hiding (not, (.), id)
import Streaming (Of, Stream)
import Streaming.Prelude (each)
import qualified Streaming.Prelude as S

--------------------------------------------------------------------------------
-- | Safely coerce between 'Expr's. This uses GHC's 'Coercible' type class,
-- where instances are only available if the underlying representations of the
-- data types are equal. This routine is useful to cast out a newtype wrapper
-- and work with the underlying data.
--
-- If the @newtype@ wrapper has a custom 'DBType' (one not derived with
-- @GeneralizedNewtypeDeriving@) this function may be unsafe and could lead to
-- runtime exceptions.
coerceExpr :: Coercible a b => Expr a -> Expr b
coerceExpr (Expr a) = Expr a


--------------------------------------------------------------------------------
dbShow :: DBType a => Expr a -> Expr Text
dbShow = unsafeCastExpr "text"


--------------------------------------------------------------------------------
unpackColumns :: Table expr haskell => O.Unpackspec expr expr
unpackColumns = O.Unpackspec (O.PackMap traversePrimExprs)


--------------------------------------------------------------------------------
-- | Given a database query, execute this query and return a 'Stream' of
-- results.
select
  :: (MonadIO m, Table rows results)
  => Connection -> O.Query rows -> Stream (Of results) m ()
select connection query = do
  results <-
    liftIO $
    O.runQueryExplicit
      queryRunner
      connection
      query
  each results

queryRunner :: Table a b => O.QueryRunner a b
queryRunner =
  O.QueryRunner (void unpackColumns)
                (const rowParser)
                (\_columns -> True) -- TODO Will we support 0-column queries?


--------------------------------------------------------------------------------
-- | Take the @LEFT JOIN@ of two tables.
leftJoin
  :: (Table lExpr lHaskell, Table rExpr rHaskell, Predicate bool)
  => (lExpr -> rExpr -> Expr bool) -- ^ The condition to join upon.
  -> O.Query lExpr -- ^ The left table
  -> O.Query rExpr -- ^ The right table
  -> O.Query (lExpr,MaybeTable rExpr)
leftJoin condition l r =
  O.leftJoinExplicit
    unpackColumns
    (unpackColumns ***! unpackColumns)
    (O.NullMaker (\(tag, t) -> MaybeTable tag t))
    l
    (liftA2 (,) (pure (lit False)) r)
    (\(a, (_, b)) -> exprToColumn (toNullableBool (condition a b)))


--------------------------------------------------------------------------------
-- TODO Suspicious! See TODO
inlineLeftJoin
  :: forall a haskell bool.
     (Table a haskell, Predicate bool)
  => O.Query a -> O.QueryArr (a -> Expr bool) (MaybeTable a)
inlineLeftJoin q =
  O.QueryArr $ \(p, left, t) ->
    let O.QueryArr rightQueryF = liftA2 (,) (pure (lit False)) q
        (right, pqR, t') = rightQueryF ((), PrimQuery.Unit, t)
        ((tag, renamed), ljPEsB) =
          O.run
            (O.runUnpackspec unpackColumns (O.extractLeftJoinFields 2 t') right)
    in ( MaybeTable tag renamed
       , PrimQuery.Join
           PrimQuery.LeftJoin
           (case toNullableBool (p renamed) of
              Expr a -> a)
           [] -- TODO !
           ljPEsB
           left
           pqR
       , t')


--------------------------------------------------------------------------------
-- | Show a type as a composite type. This is only valid for records, and
-- all fields in the record must be an instance of 'DBType'.
compositeDBType
  :: (ADTRecord t, Constraints t DBType)
  => String -- ^ The database schema name of the composite type
  -> TypeInfo t
compositeDBType n =
  TypeInfo
  { formatLit =
      catPrimExprs . gfoldMap (For :: For DBType) (pure . formatLit dbTypeInfo)
  , dbTypeName = n
  }
  where
    catPrimExprs :: [O.PrimExpr] -> O.PrimExpr
    catPrimExprs = O.FunExpr ""

-- | Lift a Haskell value into a literal database expression.
lit :: DBType a => a -> Expr a
lit = Expr . formatLit dbTypeInfo


--------------------------------------------------------------------------------
class Predicate a where
  toNullableBool :: Expr a -> Expr (Maybe Bool)

instance Predicate Bool where
  toNullableBool = toNullable

instance Predicate (Maybe Bool) where
  toNullableBool = id



--------------------------------------------------------------------------------

count :: Expr a -> Aggregate Int64
count (Expr a) = Aggregate (Just O.AggrCount) a O.AggrAll

countDistinct :: Expr a -> Aggregate Int64
countDistinct (Expr a) = Aggregate (Just O.AggrCount) a O.AggrDistinct

groupBy :: Expr a -> Aggregate a
groupBy (Expr a) = Aggregate Nothing a O.AggrAll

countStar :: Aggregate Int64
countStar = count (lit @Int64 0)

--------------------------------------------------------------------------------
class DBAvg a res | a -> res where
  avg :: Expr a -> Aggregate res
  avg (Expr a) = Aggregate (Just O.AggrAvg) a O.AggrAll

instance DBAvg Int64 Scientific
instance DBAvg Double Double
instance DBAvg Int32 Scientific
instance DBAvg Scientific Scientific
instance DBAvg Int16 Scientific

--------------------------------------------------------------------------------
-- | The class of data types that can be aggregated under the @sum@ operation.
class DBSum a res | a -> res where
  sum :: Expr a -> Aggregate b
  sum (Expr a) = Aggregate (Just O.AggrSum) a O.AggrAll

instance DBSum Int64 Scientific
instance DBSum Double Double
instance DBSum Int32 Int64
instance DBSum Scientific Scientific
instance DBSum Float Float
instance DBSum Int16 Int64

--------------------------------------------------------------------------------
class DBType a => DBMax a where
  max :: Expr a -> Aggregate a
  max (Expr a) = Aggregate (Just O.AggrMax) a O.AggrAll

instance DBMax Int64
instance DBMax Char
instance DBMax Double
instance DBMax Int32
instance DBMax Scientific
instance DBMax Float
instance DBMax Int16
instance DBMax Text
instance DBMax LocalTime
instance DBMax UTCTime
instance DBMax a => DBMax (Maybe a)

--------------------------------------------------------------------------------
class DBType a => DBMin a where
  min :: Expr a -> Aggregate a
  min (Expr a) = Aggregate (Just O.AggrMin) a O.AggrAll

instance DBMin Int64
instance DBMin Char
instance DBMin Double
instance DBMin Int32
instance DBMin Scientific
instance DBMin Float
instance DBMin Int16
instance DBMin Text
instance DBMin LocalTime
instance DBMin UTCTime
instance DBMin a => DBMin (Maybe a)

--------------------------------------------------------------------------------
boolOr :: Expr Bool -> Aggregate Bool
boolOr (Expr a) = Aggregate (Just O.AggrBoolOr) a O.AggrAll

boolAnd :: Expr Bool -> Aggregate Bool
boolAnd (Expr a) = Aggregate (Just O.AggrBoolAnd) a O.AggrAll

arrayAgg :: Expr a -> Aggregate [a]
arrayAgg (Expr a) = Aggregate (Just O.AggrArr) a O.AggrAll

stringAgg :: Expr String -> Expr String -> Aggregate String
stringAgg (Expr combiner) (Expr a) = Aggregate (Just (O.AggrStringAggr combiner)) a O.AggrAll

countRows :: O.Query a -> O.Query (Expr Int64)
countRows = fmap columnToExpr . O.countRows

class AggregateTable columns result | columns -> result, result -> columns where
  aggregator :: O.Aggregator columns result

instance AggregateTable (Aggregate a) (Expr a) where
  aggregator =
    O.Aggregator
      (O.PackMap
         (\f (Aggregate op ex dis) -> fmap Expr (f (fmap (,[],dis) op, ex))))

instance (AggregateTable a1 b1, AggregateTable a2 b2) =>
         AggregateTable (a1, a2) (b1, b2) where
  aggregator = aggregator ***! aggregator

aggregate
  :: AggregateTable table result
  => O.Query table -> O.Query result
aggregate = O.aggregate aggregator

distinct :: Table table haskell => O.Query table -> O.Query table
distinct =
  O.distinctExplicit
    (O.Distinctspec
       (O.Aggregator (O.PackMap (\f -> traversePrimExprs (\e -> f (Nothing,e))))))

--------------------------------------------------------------------------------
insert
  :: (BaseTable tableName table, MonadIO m)
  => Connection -> [table Insert] -> m Int64
insert conn rows =
  liftIO (O.runInsertMany conn tableDefinition rows)

insertReturning
  :: (BaseTable tableName table, MonadIO m)
  => Connection -> [table Insert] -> Stream (Of (table QueryResult)) m ()
insertReturning conn rows =
  do results <-
       liftIO (O.runInsertManyReturningExplicit queryRunner conn tableDefinition rows id)
     each results

insert1Returning
  :: (BaseTable tableName table,MonadIO m)
  => Connection -> table Insert -> m (table QueryResult)
insert1Returning c = fmap fromJust . S.head_ . insertReturning c . pure

update
  :: (BaseTable tableName table, Predicate bool, MonadIO m)
  => Connection
  -> (table Expr -> Expr bool)
  -> (table Expr -> table Expr)
  -> m Int64
update conn f up =
  liftIO $
  O.runUpdate
    conn
    tableDefinitionUpdate
    up
    (exprToColumn . toNullableBool . f)

updateReturning
  :: (BaseTable tableName table, Predicate bool, MonadIO m)
  => Connection
  -> (table Expr -> Expr bool)
  -> (table Expr -> table Expr)
  -> Stream (Of (table QueryResult)) m ()
updateReturning conn f up = do
  r <-
    liftIO $
    O.runUpdateReturningExplicit
      queryRunner
      conn
      tableDefinitionUpdate
      up
      (exprToColumn . toNullableBool . f)
      id
  each r

-- | Given a 'BaseTable' and a predicate, @DELETE@ all rows that match.
delete
  :: (BaseTable name table, Predicate bool)
  => Connection
  -> (table Expr -> Expr bool)
  -> IO Int64
delete conn f =
  O.runDelete conn tableDefinition (exprToColumn . toNullableBool . f)

where_ :: Predicate bool => O.QueryArr (Expr bool) ()
where_ = lmap (exprToColumn . toNullableBool) O.restrict

filterQuery :: Predicate bool => (a -> Expr bool) -> O.Query a -> O.Query a
filterQuery f q = proc _ -> do
  row <- q -< ()
  where_ -< f row
  id -< row

isNull :: Expr (Maybe a) -> Expr Bool
isNull = columnToExpr . O.isNull . exprToColumn

in_ :: DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> x ==. y ||. b) (lit False)

ilike :: Expr Text -> Expr Text -> Expr Bool
a `ilike` b =
  columnToExpr (O.binOp (O.OpOther "ILIKE") (exprToColumn a) (exprToColumn b))

--------------------------------------------------------------------------------
class Function arg res where
  -- | Build a function of multiple arguments.
  mkFunctionGo :: ([O.PrimExpr] -> O.PrimExpr) -> arg -> res

instance (DBType a, arg ~ Expr a) =>
         Function arg (Expr res) where
  mkFunctionGo mkExpr (Expr a) = Expr (mkExpr [a])

instance (DBType a, arg ~ Expr a, Function args res) =>
         Function arg (args -> res) where
  mkFunctionGo f (Expr a) = mkFunctionGo (f . (a :))

dbFunction :: Function args result => String -> args -> result
dbFunction = mkFunctionGo . O.FunExpr

nullaryFunction :: DBType a => String -> Expr a
nullaryFunction name = Expr (O.FunExpr name [])

-- | Eliminate 'PGNull' from the type of an 'Expr'. Like 'maybe' for Haskell
-- values.
nullable
  :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
nullable a f b =
  columnToExpr
    (O.matchNullable
       (exprToColumn a)
       (exprToColumn . f . columnToExpr)
       (exprToColumn b))

dbBinOp :: String -> Expr a -> Expr b -> Expr c
dbBinOp op a b =
  columnToExpr (O.binOp (O.OpOther op) (exprToColumn a) (exprToColumn b))

dbNow :: Expr UTCTime
dbNow = nullaryFunction "now"



{- $intro

   Welcome to @rel8@!

   @rel8@ is a library that builds open the fantastic @opaleye@ to query
   databases. The main objectives of @rel8@ are:

   * /Conciseness/: Users using @rel8@ should not need to write boiler-plate
     code. By using expressive types, we can provide sufficient information
     for the compiler to infer code whenever possible.

   * /Inferrable/: Despite using a lot of type level magic, it should never
     be a requirement that the user must provide a type signature to allow a
     program to compile.

   With that said, let's dive in and see an example of a program using @rel8@.

   === Required language extensions and imports

   @
   { -# LANGUAGE
         Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
         MultiParamTypeClasses #- }

   import Control.Applicative
   import Control.Arrow
   import Rel8
   @

   To use @rel8@, you will need a few language extensions:

   * @Arrows@ is necessary to use @proc@ notation. As with @opaleye@, @rel8@
     uses arrows to guarantee queries are valid.

   * @DataKinds@ is used to promote values to the type level when defining
     table/column metadata.

   * @DeriveGeneric@ is used to automatically derive functions from schema
     information.

   The others are used to provide the type system extensions needed by @rel8@.

   === Defining base tables

   In order to query a database of existing tables, we need to let @rel8@ know
   about these tables, and the schema for each table. This is done by defining
   a Haskell /record/ for each table in the database. These records should have
   a type of the form @C f name hasDefault t@. Let's see how that looks with some
   example tables:

   @
   data Part f =
     Part { partId     :: 'C' f \"PID\" ''HasDefault' Int
          , partName   :: 'C' f \"PName\" ''NoDefault' Text
          , partColor  :: 'C' f \"Color\" ''NoDefault' Int
          , partWeight :: 'C' f \"Weight\" ''NoDefault' Double
          , partCity   :: 'C' f \"City\" ''NoDefault' Text
          } deriving (Generic)

   instance 'BaseTable' "part" Part
   @

   The @Part@ table has 5 columns, each defined with the @C f ('Column ...)@
   pattern. For each column, we are specifying:

   1. The column name
   2. Whether or not this column has a default value when inserting new rows.
      In this case @partId@ does, as this is an auto-incremented primary key
      managed by the database.
   3. Whether or not the column can take @null@ values.
   4. The type of the column.

   After defining the table, we finally need to make an instance of 'BaseTable'
   so @rel8@ can query this table. By using @deriving (Generic)@, we simply need
   to write @instance BaseTable "part" Part@. The string @"part"@ here is the
   name of the table in the database (which could differ from the name of the
   type @Part@).

   === Querying tables

   With tables defined, we are now ready to write some queries. All base

-}

-- TODO
-- Query a single Expr (Maybe a) gives a conflicting fundep error

-- TODO
-- litTable :: Table expr haskell => haskell -> expr
