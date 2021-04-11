{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Aggregate
  ( count, countDistinct, countStar, countWhere
  , and, or
  , min, max
  , sum, sumWhere
  , stringAgg
  , groupByExpr
  , listAggExpr, nonEmptyAggExpr
  )
where

-- base
import Data.Int ( Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( and, max, min, null, or, sum )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregator(..), Aggregate(..), unsafeMakeAggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( caseExpr )
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr
  , fromPrimExpr
  , toPrimExpr
  )
import Rel8.Expr.Null ( null )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Type.Array ( fromPrimArray )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Num ( DBNum )
import Rel8.Type.Ord ( DBMax, DBMin )
import Rel8.Type.String ( DBString )
import Rel8.Type.Sum ( DBSum )


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
count :: (i -> Expr a) -> Aggregator i (Expr Int64)
count = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregate
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Count the number of distinct occurances of a single column. Corresponds to
-- @COUNT(DISTINCT a)@
countDistinct :: Sql DBEq a => (i -> Expr a) -> Aggregator i (Expr Int64)
countDistinct = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregate
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrDistinct
    }


-- | Corresponds to @COUNT(*)@.
countStar :: Aggregator i (Expr Int64)
countStar = count \_ -> litExpr True


-- | A count of the number of times a given expression is @true@.
countWhere :: (i -> Expr Bool) -> Aggregator i (Expr Int64)
countWhere condition = count \i -> caseExpr [(condition i, litExpr (Just True))] null


-- | Corresponds to @bool_and@.
and :: (i -> Expr Bool) -> Aggregator i (Expr Bool)
and = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregate
    { operation = Opaleye.AggrBoolAnd
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Corresponds to @bool_or@.
or :: (i -> Expr Bool) -> Aggregator i (Expr Bool)
or = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregate
    { operation = Opaleye.AggrBoolOr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Produce an aggregation for @Expr a@ using the @max@ function.
max :: Sql DBMax a => (i -> Expr a) -> Aggregator i (Expr a)
max = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregate
    { operation = Opaleye.AggrMax
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Produce an aggregation for @Expr a@ using the @max@ function.
min :: Sql DBMin a => (i -> Expr a) -> Aggregator i (Expr a)
min = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregate
    { operation = Opaleye.AggrMin
    , ordering = []
    , distinction = Opaleye.AggrAll
    }

-- | Corresponds to @sum@. Note that in SQL, @sum@ is type changing - for
-- example the @sum@ of @integer@ returns a @bigint@. Rel8 doesn't support
-- this, and will add explicit cast back to the original input type. This can
-- lead to overflows, and if you anticipate very large sums, you should upcast
-- your input.
sum :: Sql DBSum a => (i -> Expr a) -> Aggregator i (Expr a)
sum = unsafeMakeAggregate toPrimExpr (castExpr . fromPrimExpr) $
  Just Aggregate
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Take the sum of all expressions that satisfy a predicate.
sumWhere :: (Sql DBNum a, Sql DBSum a)
  => (i -> Expr Bool) -> (i -> Expr a) -> Aggregator i (Expr a)
sumWhere condition f = sum \i -> (caseExpr [(condition i, f i)] 0)


-- | Corresponds to @string_agg()@.
stringAgg :: Sql DBString a
  => Expr db -> (i -> Expr a) -> Aggregator i (Expr a)
stringAgg delimiter =
  unsafeMakeAggregate toPrimExpr (castExpr . fromPrimExpr) $
    Just Aggregate
      { operation = Opaleye.AggrStringAggr (toPrimExpr delimiter)
      , ordering = []
      , distinction = Opaleye.AggrAll
      }


-- | Aggregate a value by grouping by it.
groupByExpr :: Sql DBEq a => (i -> Expr a) -> Aggregator i (Expr a)
groupByExpr = unsafeMakeAggregate toPrimExpr fromPrimExpr Nothing


-- | Collect expressions values as a list.
listAggExpr :: (i -> Expr a) -> Aggregator i (Expr [a])
listAggExpr = unsafeMakeAggregate toPrimExpr from $ Just
  Aggregate
    { operation = Opaleye.AggrArr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }
  where
    from = fromPrimExpr . fromPrimArray


-- | Collect expressions values as a non-empty list.
nonEmptyAggExpr :: (i -> Expr a) -> Aggregator i (Expr (NonEmpty a))
nonEmptyAggExpr = unsafeMakeAggregate toPrimExpr from $ Just
  Aggregate
    { operation = Opaleye.AggrArr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }
  where
    from = fromPrimExpr . fromPrimArray
