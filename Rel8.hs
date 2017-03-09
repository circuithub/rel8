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
  , leftJoinA
  , unionAll
  , O.exists, O.notExists

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
  , count, groupBy, DBSum(..), countStar, DBMin(..), DBMax(..), DBAvg(..)
  , boolAnd, boolOr, stringAgg, arrayAgg, countDistinct
  , countRows, Aggregate

    -- * Tables
  , Table
  , MaybeTable, isTableNull
  , Col(..)

    -- * Expressions
  , Expr, coerceExpr, dbShow, case_

    -- ** Equality
  , DBEq, (==.), (?=.), in_, ilike

    -- ** Ordering
  , DBOrd, (>.), (>=.), (<.), (<=.)

    -- ** Numeric Operators
  , (+), (-), negate, (*)

    -- ** Boolean-valued expressions
  , DBBool(..)

    -- ** Literals
  , DBType(..), lit, dbNow
  , TypeInfo(..), showableDbType, compositeDBType

    -- ** Null
  , ToNullable(toNullable) , ($?), isNull, nullable


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

    -- * Interpretations
  , QueryResult, Schema, Anon

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
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import qualified Opaleye.Binary as O
import qualified Opaleye.Internal.Binary as O
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
    unpackColumns
    (O.NullMaker (\(tag, t) -> MaybeTable tag t))
    l
    (liftA2 (,) (pure (lit (Just False))) r)
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
leftJoinA
  :: (Table a haskell, DBBool bool)
  => O.Query a -> O.QueryArr (a -> Expr bool) (MaybeTable a)
leftJoinA q =
  O.QueryArr $ \(p, left, t) ->
    let O.QueryArr rightQueryF = liftA2 (,) (pure (lit (Just False))) q
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


dbBinOp :: String -> Expr a -> Expr b -> Expr c
dbBinOp op a b =
  columnToExpr (O.binOp (O.OpOther op) (exprToColumn a) (exprToColumn b))

-- | Corresponds to the @now()@ function.
dbNow :: Expr UTCTime
dbNow = nullaryFunction "now"

-- | Take the union of all rows in the first query and all rows in the second
-- query. Corresponds to the PostgreSQL @UNION ALL@ operator.
unionAll :: Table table haskell => O.Query table -> O.Query table -> O.Query table
unionAll = O.unionAllExplicit (O.Binaryspec (O.PackMap traverseBinary))

{- $intro

   Welcome to @rel8@!

   @rel8@ is a library that builds open the fantastic @opaleye@ library to
   query databases, and provides a slightly alternative API. The main objectives
   of @rel8@ are:

   * /Conciseness/: Users using @rel8@ should not need to write boiler-plate
     code. By using expressive types, we can provide sufficient information
     for the compiler to infer code whenever possible.

   * /Inferrable/: Despite using a lot of type level magic, it should never
     be a requirement that the user must provide a type signature to allow a
     program to compile.

   * /Compatible/: @rel8@ tries to use the existing @opaleye@ API as much as
     possible.

   Now, let's dive in and see an example of a program using @rel8@.

   === Required language extensions and imports

   @
   { -# LANGUAGE Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
                 OverloadedStrings #- }

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
   data Part f = Part
     { partId     :: 'C' f \"PID\" ''HasDefault' Int
     , partName   :: 'C' f \"PName\" ''NoDefault' String
     , partColor  :: 'C' f \"Color\" ''NoDefault' Int
     , partWeight :: 'C' f \"Weight\" ''NoDefault' Double
     , partCity   :: 'C' f \"City\" ''NoDefault' String
     } deriving (Generic)

   instance 'BaseTable' Part where 'tableName' = \"part\"
   @

   The @Part@ table has 5 columns, each defined with the @C f ..@
   pattern. For each column, we are specifying:

   1. The column name
   2. Whether or not this column has a default value when inserting new rows.
      In this case @partId@ does, as this is an auto-incremented primary key
      managed by the database.
   3. The type of the column.

   After defining the table, we finally need to make an instance of 'BaseTable'
   so @rel8@ can query this table. By using @deriving (Generic)@, we simply need
   to write @instance BaseTable Part where tableName = "part"@.

   === Querying tables

   With tables defined, we are now ready to write some queries. All 'BaseTable's
   give rise to a query - the query of all rows in that table:

   @
   allParts :: 'O.Query' (Part 'Expr')
   allParts = queryTable
   @

   Notice the type of @allParts@ specifies that we're working with @Part Expr@.
   This means that the contents of the @Part@ record will contain expressions -
   one for each column in the table. As 'O.Query' is a 'Functor', we can derive
   a new query for all part cities in the database:

   @
   allPartCities :: 'O.Query' ('Expr' String)
   allPartCities = partCity \<$\> allParts
   @

   Now we have a query containing just one column - expressions of type 'String'.

   === @WHERE@ clauses

   Usually when we are querying database, we are querying for subsets of
   information. In SQL, we apply predicates using @WHERE@ - and @rel8@ supports
   this too, in two forms.

   Firstly, we can use 'filterQuery', similar to how we would use 'filter':

   @
   londonParts :: 'O.Query' (Part 'Expr')
   londonParts = 'filterQuery' (\\p -> partCity p '==.' \"London\") allParts
   @

   'filterQuery' takes a function from rows in a query to a predicate. In this
   case we can use '==.' to compare to expressions for equality. On the left,
   @partCity p :: Expr String@, and on the right @\"London\" :: Expr String@ (
   the literal string @London@).

   Alternatively, we can use 'where_' with arrow notation, which is similar to
   using 'guard' with 'MonadPlus':

   @
   heavyParts :: 'O.Query' (Part 'Expr')
   heavyParts = proc _ -> do
     part <- 'queryTable' -< ()
     'where_' -\< partWeight part '>.' 5
     returnA -< part
   @

   == Joining Queries

   @rel8@ supports joining multiple queries into one, in a few different ways.

   === Products and Inner Joins

   We can take the product of two queries - each row of the first query
   paired with each row of the second query - by sequencing queries inside a
   'O.Query'. Let's introduce another table:

   @
   data Supplier f = Supplier
     { supplierId :: 'C' f \"SID\" ''HasDefault' Int
     , supplierName :: 'C' f \"SName\" ''NoDefault' String
     , supplierStatus :: 'C' f \"Status\" ''NoDefault' Int
     , supplierCity :: 'C' f \"City\" ''NoDefault' String
     } deriving (Generic)

   instance 'BaseTable' Supplier where 'tableName' = "supplier"
   @

   We can take the product of all parts paired against all suppliers:

   @
   allPartsAndSuppliers :: 'O.Query' (Part 'Expr', Supplier 'Expr')
   allPartsAndSuppliers = proc _ -> do
     part <- 'queryTable' -< ()
     supplier <- 'queryTable' -< ()
     returnA -< (part, supplier)
   @

   We could write this a little more succinctly using using the @Applicative@
   instance for 'O.Query', as '<*>' corresponds to products:

   @
   allPartsAndSuppliers2 :: 'O.Query' (Part 'Expr', Supplier 'Expr')
   allPartsAndSuppliers2 = liftA2 (,) 'queryTable' 'queryTable'
   @

   In both queries, we've just used 'queryTable' to select the necessary rows.
   'queryTable' is overloaded, but by knowing the type of rows to select, it
   will change which table it queries from.

   We can combine products with the techniques we've just seen in order to
   produce the inner join of two tables. For example, here is a query to pair
   up each part with all suppliers in the same city.

   @
   partsAndSuppliers :: Query (Part Expr, Supplier Expr)
   partsAndSuppliers =
     'filterQuery'
       (\(part, supplier) -> partCity part '==.' supplierCity supplier)
       allPartsAndSuppliers
   @

   === Left Joins

   The previous query gave us parts with /at least one/ supplier in the same
   city. If a part has no suppliers in the same city, it will be omitted from
   the results. But what if we needed this information? In SQL we can capture
   this with a @LEFT JOIN@, and @rel8@ supports this.

   Left joins can be introduced with the 'leftJoin', which takes two queries,
   or using arrow notation with `leftJoinA`. Let's look at the latter, as it
   is often more concise.

   @
   partsAndSuppliersLJ :: Query (Part Expr, MaybeTable (Supplier Expr))
   partsAndSuppliersLJ = proc _ -> do
     part <- queryTable -< ()
     maybeSupplier
       <- leftJoinA queryTable
       -\< \\supplier -> partCity part ==. supplierCity supplier
     returnA -< (part, maybeSupplier)
   @

   This is a little different to anything we've seen so far, so let's break it
   down. 'leftJoinA' takes as its first argument the query to join in. In this
   case we just use 'queryTable' to select all supplier rows. @LEFT JOIN@s
   also require a predicate, and we supply this as /input/ to @leftJoinA@. The
   input is itself a function, a function from rows in the to-be-joined table
   to booleans. Notice that in this predicate, we are free to refer to tables and
   columns already in the query (as @partCity part@ is not part of the supplier
   table).

   Left joins themselves can be filtered, as they are just another query.
   However, the results of a left join are wrapped in 'MaybeTable', which
   indicates that /all/ of the columns in this table might be null, if the
   join failed to match any rows. We can use this information with our
   @partsAndSuppliersLJ@ query to find parts where there are no suppliers in the
   same city:

   @
   partsWithoutSuppliersInCity :: Query (Part Expr)
   partsWithoutSuppliersInCity = proc _ -> do
     (part, maybeSupplier) <- partsAndSuppliersLJ -< ()
     where_ -< isNull (maybeSupplier $? supplierId)
     returnA -< part
   @

   We are filtering our query for suppliers where the id is null. Ordinarily
   this would be a type error - we declared that @supplierId@ contains @Int@,
   rather than @Maybe Int@. However, because suppliers come from a left join,
   when we project out from 'MaybeTable' /all/ columns become nullable. It may
   help to think of @($?)@ as having the type:

   @
   ($?) :: (a -> Expr b) -> MaybeTable a -> Expr (Maybe b)
   @

   though in @rel8@ we're a little bit more general.

-}
