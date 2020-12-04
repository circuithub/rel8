{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Internal.Aggregate where

import Control.Category ((.))
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime, LocalTime)
import Data.Vector (Vector)
import qualified Opaleye.Aggregate as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.QueryArr as O
import Prelude hiding (not, (.), id)
import Rel8.Internal.DBType
import Rel8.Internal.Expr
import Rel8.Internal.Table
import Rel8.Internal.Types

--------------------------------------------------------------------------------
-- | The class of data types that can be aggregated under the @avg@ operation.
-- This type class contains two parameters, as @avg@ can be a type-changing
-- operation in PostgreSQL.
class DBAvg a res | a -> res where
  -- | Corresponds to @avg@.
  avg :: Expr a -> Aggregate res
  avg (Expr a) = Aggregate (Just (O.AggrAvg, [], O.AggrAll)) a

instance DBAvg Int64 Scientific
instance DBAvg Double Double
instance DBAvg Int32 Scientific
instance DBAvg Scientific Scientific
instance DBAvg Int16 Scientific

--------------------------------------------------------------------------------
-- | The class of data types that can be aggregated under the @sum@ operation.
-- This type class contains two parameters, as @sum@ can be a type-changing
-- operation in PostgreSQL.
class DBSum a res | a -> res where
  -- | Corresponds to @sum@.
  sum :: Expr a -> Aggregate res
  sum (Expr a) = Aggregate (Just (O.AggrSum, [], O.AggrAll)) a

instance DBSum Int64 Scientific
instance DBSum Double Double
instance DBSum Int32 Int64
instance DBSum Scientific Scientific
instance DBSum Float Float
instance DBSum Int16 Int64


--------------------------------------------------------------------------------
-- | The class of data types that can be aggregated under the @max@ operation.
class DBType a => DBMax a where
  -- | Corresponds to @max@.
  max :: Expr a -> Aggregate a
  max (Expr a) = Aggregate (Just (O.AggrMax, [], O.AggrAll)) a

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
-- | The class of data types that can be aggregated under the @min@ operation.
class DBType a => DBMin a where
  -- | Corresponds to @min@.
  min :: Expr a -> Aggregate a
  min (Expr a) = Aggregate (Just (O.AggrMin, [], O.AggrAll)) a

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
-- | Aggregate with @bool_or@.
boolOr :: Expr Bool -> Aggregate Bool
boolOr (Expr a) = Aggregate (Just (O.AggrBoolOr, [], O.AggrAll)) a

-- | Aggregate with @bool_and@.
boolAnd :: Expr Bool -> Aggregate Bool
boolAnd (Expr a) = Aggregate (Just (O.AggrBoolAnd, [], O.AggrAll)) a

-- | Aggregate with @array_agg@.
arrayAgg :: Expr a -> Aggregate (Vector a)
arrayAgg (Expr a) = Aggregate (Just (O.AggrArr, [], O.AggrAll)) a

-- | Aggregate with @string_agg@.
stringAgg :: Expr String -> Expr String -> Aggregate String
stringAgg (Expr combiner) (Expr a) = Aggregate (Just ((O.AggrStringAggr combiner), [], O.AggrAll)) a

-- | Count how many rows are in a query. If the query is empty, this still returns
-- the result @0@.
countRows :: O.Query a -> O.Query (Expr Int64)
countRows = fmap columnToExpr . O.countRows

-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
count :: Expr a -> Aggregate Int64
count (Expr a) = Aggregate (Just (O.AggrCount, [], O.AggrAll)) a

-- | Count distinct occurances of a single column. Corresponds to
-- @COUNT(DISTINCT a)@.
countDistinct :: Expr a -> Aggregate Int64
countDistinct (Expr a) = Aggregate (Just (O.AggrCount, [], O.AggrDistinct)) a

-- | Group by distinct values in this column. Corresponds to @GROUP BY@.
groupBy :: Expr a -> Aggregate a
groupBy (Expr a) = Aggregate Nothing a

-- | Corresponds to @COUNT(*)@.
countStar :: Aggregate Int64
countStar = count (lit @Int64 0)
