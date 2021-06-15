{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
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
  , slistAggExpr, snonEmptyAggExpr
  )
where

-- base
import Data.Int ( Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( and, max, min, null, or, sum )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate, Aggregator(..), unsafeMakeAggregate )
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
import Rel8.Schema.Null ( Sql, Unnullify )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Array ( encodeArrayElement )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information ( TypeInformation )
import Rel8.Type.Num ( DBNum )
import Rel8.Type.Ord ( DBMax, DBMin )
import Rel8.Type.String ( DBString )
import Rel8.Type.Sum ( DBSum )


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
count :: Expr a -> Aggregate (Expr Int64)
count = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Count the number of distinct occurances of a single column. Corresponds to
-- @COUNT(DISTINCT a)@
countDistinct :: Sql DBEq a => Expr a -> Aggregate (Expr Int64)
countDistinct = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrDistinct
    }


-- | Corresponds to @COUNT(*)@.
countStar :: Aggregate (Expr Int64)
countStar = count (litExpr True)


-- | A count of the number of times a given expression is @true@.
countWhere :: Expr Bool -> Aggregate (Expr Int64)
countWhere condition = count (caseExpr [(condition, litExpr (Just True))] null)


-- | Corresponds to @bool_and@.
and :: Expr Bool -> Aggregate (Expr Bool)
and = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrBoolAnd
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Corresponds to @bool_or@.
or :: Expr Bool -> Aggregate (Expr Bool)
or = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrBoolOr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Produce an aggregation for @Expr a@ using the @max@ function.
max :: Sql DBMax a => Expr a -> Aggregate (Expr a)
max = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrMax
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Produce an aggregation for @Expr a@ using the @max@ function.
min :: Sql DBMin a => Expr a -> Aggregate (Expr a)
min = unsafeMakeAggregate toPrimExpr fromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrMin
    , ordering = []
    , distinction = Opaleye.AggrAll
    }

-- | Corresponds to @sum@. Note that in SQL, @sum@ is type changing - for
-- example the @sum@ of @integer@ returns a @bigint@. Rel8 doesn't support
-- this, and will add explicit cast back to the original input type. This can
-- lead to overflows, and if you anticipate very large sums, you should upcast
-- your input.
sum :: Sql DBSum a => Expr a -> Aggregate (Expr a)
sum = unsafeMakeAggregate toPrimExpr (castExpr . fromPrimExpr) $
  Just Aggregator
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


-- | Take the sum of all expressions that satisfy a predicate.
sumWhere :: (Sql DBNum a, Sql DBSum a)
  => Expr Bool -> Expr a -> Aggregate (Expr a)
sumWhere condition a = sum (caseExpr [(condition, a)] 0)


-- | Corresponds to @string_agg()@.
stringAgg :: Sql DBString a
  => Expr db -> Expr a -> Aggregate (Expr a)
stringAgg delimiter =
  unsafeMakeAggregate toPrimExpr (castExpr . fromPrimExpr) $
    Just Aggregator
      { operation = Opaleye.AggrStringAggr (toPrimExpr delimiter)
      , ordering = []
      , distinction = Opaleye.AggrAll
      }


-- | Aggregate a value by grouping by it.
groupByExpr :: Sql DBEq a => Expr a -> Aggregate (Expr a)
groupByExpr = unsafeMakeAggregate toPrimExpr fromPrimExpr Nothing


-- | Collect expressions values as a list.
listAggExpr :: Sql DBType a => Expr a -> Aggregate (Expr [a])
listAggExpr = slistAggExpr typeInformation


-- | Collect expressions values as a non-empty list.
nonEmptyAggExpr :: Sql DBType a => Expr a -> Aggregate (Expr (NonEmpty a))
nonEmptyAggExpr = snonEmptyAggExpr typeInformation


slistAggExpr :: ()
  => TypeInformation (Unnullify a) -> Expr a -> Aggregate (Expr [a])
slistAggExpr info = unsafeMakeAggregate to fromPrimExpr $ Just
  Aggregator
    { operation = Opaleye.AggrArr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }
  where
    to = encodeArrayElement info . toPrimExpr


snonEmptyAggExpr :: ()
  => TypeInformation (Unnullify a) -> Expr a -> Aggregate (Expr (NonEmpty a))
snonEmptyAggExpr info = unsafeMakeAggregate to fromPrimExpr $ Just
  Aggregator
    { operation = Opaleye.AggrArr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }
  where
    to = encodeArrayElement info . toPrimExpr
