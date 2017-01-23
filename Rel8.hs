{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , BaseTable(tableName)

    -- * Querying Tables
  , O.Query, O.QueryArr
  , queryTable
  , leftJoin
  , inlineLeftJoinA

    -- ** Filtering
  , where_
  , filterQuery
  , distinct

    -- ** Offset and limit
  , O.limit
  , O.offset

    -- ** Ordering
  , asc, desc, orderNulls, O.orderBy, OrderNulls(..)

    -- * Aggregation
  , aggregate
  , AggregateTable
  , count, groupBy, DBSum(..), countStar, DBMin(..), DBMax(..), avg
  , boolAnd, boolOr, stringAgg, arrayAgg, countDistinct
  , countRows, Aggregate

    -- * Tables
  , Table
  , MaybeTable
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
  , DBBool(..)

    -- ** Literals
  , DBType(..), lit, dbNow
  , TypeInfo(..), showableDbType, compositeDBType

    -- ** Null
  , toNullable , (?), isNull, nullable


    -- * Running Queries
    -- ** @SELECT@
  , select

    -- ** @INSERT@
  , Default(..), Insert
  , insert, insert1Returning {- , insertReturning -}

    -- ** @UPDATE@
  , update {- , updateReturning -}

    -- ** @DELETE@
  , delete

    -- * Re-exported symbols
  , Connection, Stream, Of, Generic

    -- * Unsafe routines
  , unsafeCoerceExpr
  , unsafeCastExpr
  , dbFunction
  , nullaryFunction
  , dbBinOp
  ) where

import Rel8.Internal.DBType
import Control.Monad.Rel8
import Rel8.Internal

import Control.Applicative (liftA2)
import Control.Category ((.), id)
import Data.List (foldl')
import Data.Profunctor (lmap)
import Data.Profunctor.Product ((***!))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.Distinct as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Join as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.PrimQuery as PrimQuery
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Unpackspec as O
import qualified Opaleye.Join as O
import qualified Opaleye.Operators as O
import qualified Opaleye.Order as O
import Prelude hiding (not, (.), id)
import Streaming (Of, Stream)


--------------------------------------------------------------------------------
-- | Take the @LEFT JOIN@ of two queries.
leftJoin
  :: (Table left a, Table right b, DBBool bool)
  => (left -> right -> Expr bool) -- ^ The condition to join upon.
  -> O.Query left -- ^ The left table
  -> O.Query right -- ^ The right table
  -> O.Query (left,MaybeTable right)
leftJoin condition l r =
  O.leftJoinExplicit
    unpackColumns
    (unpackColumns ***! unpackColumns)
    (O.NullMaker (\(tag, t) -> MaybeTable tag t))
    l
    (liftA2 (,) (pure (lit False)) r)
    (\(a, (_, b)) -> exprToColumn (toNullable (condition a b)))


--------------------------------------------------------------------------------
-- | A more convenient form of 'leftJoin' when using arrow notation.
-- @inlineLeftJoinA@ takes the left join of all proceeding queries against a
-- given query. The input to the 'QueryArr' is a predicate function against
-- rows in the to-be-joined query.
--
-- === __Example__
-- @
-- -- Return all users and comments, including users who haven't made a comment.
-- usersAndComments :: Query (User Expr, MaybeTable (Comment Expr))
-- proc _ -> do
--   u <- queryTable -< ()
--   comment <- inlineLeftJoinA -< \c -> commentUser c ==. userId u
--   returnA (u, c)
-- @
inlineLeftJoinA
  :: (Table a haskell, DBBool bool)
  => O.Query a -> O.QueryArr (a -> Expr bool) (MaybeTable a)
inlineLeftJoinA q =
  O.QueryArr $ \(p, left, t) ->
    let O.QueryArr rightQueryF = liftA2 (,) (pure (lit False)) q
        (right, pqR, t') = rightQueryF ((), PrimQuery.Unit, t)
        ((tag, renamed), ljPEsB) =
          O.run
            (O.runUnpackspec unpackColumns (O.extractLeftJoinFields 2 t') right)
    in ( MaybeTable tag renamed
       , PrimQuery.Join
           PrimQuery.LeftJoin
           (case toNullable (p renamed) of
              Expr a -> a)
           [] -- TODO ?
           ljPEsB
           left
           pqR
       , t')

-- | Take only distinct rows in a 'O.Query'. This maps to grouping by every
-- column in the table.
distinct :: Table table haskell => O.Query table -> O.Query table
distinct =
  O.distinctExplicit
    (O.Distinctspec
       (O.Aggregator (O.PackMap (\f -> traversePrimExprs (\e -> f (Nothing,e))))))

-- | Restrict a 'O.QueryArr' to only contain rows that satisfy a given predicate.
where_ :: DBBool bool => O.QueryArr (Expr bool) ()
where_ = lmap (exprToColumn . toNullable) O.restrict

-- | Filter a 'O.Query' into a new query where all rows satisfy a given
-- predicate.
filterQuery :: DBBool bool => (a -> Expr bool) -> O.Query a -> O.Query a
filterQuery f q = proc _ -> do
  row <- q -< ()
  where_ -< f row
  id -< row

-- | Corresponds to the @IS NULL@ operator.
isNull :: Expr (Maybe a) -> Expr Bool
isNull = columnToExpr . O.isNull . exprToColumn

-- | Test if an 'Expr' is in a list of 'Expr's. This is performed by folding
-- '==.' over all values and combining them with '||.'.
in_ :: DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> x ==. y ||. b) (lit False)

-- | Corresponds to the @ILIKE@ operator.
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
