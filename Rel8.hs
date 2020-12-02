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
  ( -- $welcome

    -- * Defining Tables

    -- $defining

    C
  , Anon
  , HasDefault(..)
  , BaseTable(tableName, tableSchema)
  , Table
  , TableC
  , TableType(..)
  , ExprType
  , HigherKindedTable

    -- * Querying Tables
  , O.Query
  , queryTable
  , leftJoin
  , fullJoin
  , unionAll
  , exceptAll
  , O.restrictExists, O.restrictNotExists

    -- ** Filtering
  , where_
  , filterQuery
  , distinct

    -- ** Offset and limit
  , O.limit
  , O.offset

    -- ** Ordering
  , asc, desc, orderNulls, O.orderBy, OrderNulls(..)
  , distinctOn, distinctOnBy

    -- * Aggregation
  , aggregate
  , AggregateTable
  , count, groupBy, DBSum(..), countStar, DBMin(..), DBMax(..), DBAvg(..)
  , boolAnd, boolOr, stringAgg, arrayAgg, countDistinct
  , countRows, Aggregate

    -- * Tables
  , bool
  , MaybeTable, isTableNull, maybeTable
  , TheseTable, theseTable

    -- * Expressions
  , Expr, coerceExpr, dbShow, case_

    -- ** Equality
  , DBEq, (==.), in_, ilike

    -- ** Ordering
  , DBOrd, (>.), (>=.), (<.), (<=.)

    -- ** Numeric Operators
  , (+), (-), negate, (*)

    -- ** Booleans
  , (&&.), (||.), not_, Predicate

    -- ** Literals
  , DBType(..), lit, dbNow
  , TypeInfo(..), showableDbType, compositeDBType

    -- ** Null
  , ToNullable(toNullable) , ($?), isNull, nullable
  , liftOpNull, mapNull

    -- *** Null-lifted operators
    -- $nullLift

  , (==?), (<?), (<=?), (>?), (>=?)
  , (||?), (&&?), (+?), (*?), (-?)
  , (/?)

    -- * Values
  , values

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
  , QueryResult, SchemaInfo

    -- * Re-exported symbols
  , Connection, Stream, Of, Generic

    -- * Unsafe routines
  , unsafeCoerceExpr
  , unsafeCastExpr
  , unsafeLiteral
  , dbFunction
  , nullaryFunction
  , dbBinOp
  ) where

import Control.Applicative (liftA2)
import Control.Arrow (app)
import Control.Category ((.), id)
import Control.Monad.Rel8
import Data.List (foldl')
import Data.Profunctor (lmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import qualified Opaleye.Binary as O
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.Distinct as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Join as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Join as O
import qualified Opaleye.Operators as O
import qualified Opaleye.Order as O
import qualified Opaleye.Values as O
import Prelude hiding (not, (.), id)
import Rel8.Internal
import Streaming (Of, Stream)

infix 4 ==? , <? , <=? , >? , >=?
infixr 2 ||?
infixr 3 &&?
infixl 7 *?
infixl 6 +?, -?
infixr 7 /?

--------------------------------------------------------------------------------
(==?)
  :: (DBEq a, ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe Bool)
a ==? b = liftOpNull (==.) (toNullable a) (toNullable b)

(<?), (<=?), (>?), (>=?)
  :: (DBOrd a, ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe Bool)
a <? b = liftOpNull (<.) (toNullable a) (toNullable b)
a <=? b = liftOpNull (<=.) (toNullable a) (toNullable b)
a >? b = liftOpNull (>.) (toNullable a) (toNullable b)
a >=? b = liftOpNull (>=.) (toNullable a) (toNullable b)

(||?), (&&?)
  :: (ToNullable bool1 (Maybe Bool), ToNullable bool2 (Maybe Bool))
  => Expr bool1 -> Expr bool2 -> Expr (Maybe Bool)
a ||? b = liftOpNull (||.) (toNullable a) (toNullable b)
a &&? b = liftOpNull (&&.) (toNullable a) (toNullable b)

(+?), (*?), (-?)
  :: (Num (Expr a), ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe a)
a +? b = liftOpNull (+) (toNullable a) (toNullable b)
a *? b = liftOpNull (*) (toNullable a) (toNullable b)
a -? b = liftOpNull (-) (toNullable a) (toNullable b)

(/?)
  :: (Fractional (Expr a), ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe a)
a /? b = liftOpNull (/) (toNullable a) (toNullable b)

--------------------------------------------------------------------------------
unsafeLiteral :: forall a. String -> Expr a
unsafeLiteral = columnToExpr @a @a . O.Column . O.ConstExpr . O.OtherLit

--------------------------------------------------------------------------------
-- | Take the @LEFT JOIN@ of two queries.
leftJoin
  :: (Table left a, Table right b, Predicate bool)
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
-- | Take the @FULL OUTER JOIN@ of two queries.
fullJoin
  :: (Table left a, Table right b, Predicate bool)
  => (left -> right -> Expr bool) -- ^ The condition to join upon.
  -> O.Query left -- ^ The left table
  -> O.Query right -- ^ The right table
  -> O.Query (TheseTable left right)
fullJoin condition l r = uncurry TheseTable <$>
  O.fullJoinExplicit
    unpackColumns
    unpackColumns
    (O.NullMaker (\(tag, t) -> MaybeTable tag t))
    (O.NullMaker (\(tag, t) -> MaybeTable tag t))
    (liftA2 (,) (pure (lit (Just False))) l)
    (liftA2 (,) (pure (lit (Just False))) r)
    (\((_, a), (_, b)) -> exprToColumn (toNullable (condition a b)))


-- | Take only distinct rows in a 'O.Query'. This maps to grouping by every
-- column in the table.
distinct :: Table table haskell => O.Query table -> O.Query table
distinct =
  O.distinctExplicit
    (O.Distinctspec
       (O.Aggregator (O.PackMap (\f -> traversePrimExprs (\e -> f (Nothing,e))))))

-- | Restrict a 'O.QueryArr' to only contain rows that satisfy a given predicate.
where_ :: Predicate bool => Expr bool -> O.Query ()
where_ x = lmap (const (exprToColumn (toNullable x))) O.restrict

-- | Filter a 'O.Query' into a new query where all rows satisfy a given
-- predicate.
filterQuery :: Predicate bool => (a -> Expr bool) -> O.Query a -> O.Query a
filterQuery f q = proc _ -> do
  row <- q -< ()
  app -< (where_ (f row), ())
  id -< row

-- | Corresponds to the @IS NULL@ operator.
isNull :: Expr (Maybe a) -> Expr Bool
isNull = columnToExpr . O.isNull . exprToColumn

-- | Test if an 'Expr' is in a list of 'Expr's. This is performed by folding
-- '==.' over all values and combining them with '||.'.
in_ :: DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> b ||. x ==. y) (lit False)

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
unionAll = O.unionAllExplicit binaryColumns


-- | Take all of the rows in the first query minus all the rows in the second
-- query. Corresponds to the PostgreSQL @EXCEPT ALL@ operator.
exceptAll :: Table table haskell => O.Query table -> O.Query table -> O.Query table
exceptAll = O.exceptAllExplicit binaryColumns


values :: Table table haskell => [table] -> O.Query table
values = O.valuesUnsafeExplicit unpackColumns valuesColumns

{- $welcome

Welcome to Rel8! Rel8 is an API built on top of the fantastic Opaleye library to
provide an easy and type-safe way to interact with relational databases.

The main objectives of Rel8 are:

* Conciseness: Users using Rel8 should not need to write boiler-plate code. By
  using expressive types, we can provide sufficient information for the compiler
  to infer code whenever possible.

* Inferrable: Despite using a lot of type level magic, it should never be a
  requirement that the user must provide a type signature to allow a program to compile.

* Compatible: Rel8 tries to use the existing Opaleye API as much as possible.

If you're new to Rel8, you're encouraged to check out the documentation over at
<https://rel8.readthedocs.io/en/latest/ Read The Docs>, where a comprehensive
Getting Started guide is provided, along with a comparison with traditional
Opaleye code.

-}

{- $defining

The idiomatic way to define tables in Rel8 is to use a Haskell record. However,
as a table might be used in a query, or in query results, we need to two
different "viwes" on this record. We achieve this by parameterising the record
with a particular interpretation. This is done by providing a type parameter,
and then using the 'C' type family to map this interpretation to a particular
Haskell type.

For example, we might have a record for a Haskell library on Hackage:

@
data Library f = Library
  { libraryName :: C f "name" 'NoDefault Text
  , libraryUploadedAt :: C f "uploaded_at" 'NoDefault UTCTime
  }
@

To use this type with Rel8, we just need to define a few instances. This is
boiler plate code, and it can be automated with GHC Generics. The final
type definition is:

@
data Library f = Library
  { libraryName :: C f "name" 'NoDefault Text
  , libraryUploadedAt :: C f "uploaded_at" 'NoDefault UTCTime
  } deriving (Generic)

instance (expr ~ Expr, result ~ QueryResult) => Table (Library expr) (Library result)
instance BaseTable Library where tableName = "hackage_libraryr"
@

(The strange instance declaration may look a bit scary, but will give you /much/
better type inference! This is discussed in a bit more detail
<https://rel8.readthedocs.io/en/latest/concepts.html#even-more-type-inference here>)


-}

{- $nullLift

Null-lifted operators work like their conventional operators, but they also
allow @null@ as either argument. These functions are defined in the PostgreSQL
engine as short-circuiting with a return value of @nulL@ if either argument is
@null@.

You may find these operators convenient when mixing nullable types.

-}
