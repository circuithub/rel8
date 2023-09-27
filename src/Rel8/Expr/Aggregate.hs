{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Aggregate
  ( count, countOn, countStar
  , countDistinct, countDistinctOn
  , countWhere, countWhereOn
  , and, andOn, or, orOn
  , min, minOn, max, maxOn
  , sum, sumOn, sumWhere
  , avg, avgOn
  , stringAgg, stringAggOn
  , groupByExpr, groupByExprOn
  , distinctAggregate
  , filterWhereExplicit
  , listAggExpr, listAggExprOn, nonEmptyAggExpr, nonEmptyAggExprOn
  , listCatExpr, listCatExprOn, nonEmptyCatExpr, nonEmptyCatExprOn
  , slistAggExpr, snonEmptyAggExpr
  , slistCatExpr, snonEmptyCatExpr
  )
where

-- base
import Data.Int ( Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.String (IsString)
import Prelude hiding (and, max, min, null, or, show, sum)

-- opaleye
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Operators as Opaleye

-- profunctors
import Data.Profunctor (dimap, lmap)

-- rel8
import Rel8.Aggregate
  ( Aggregator' (Aggregator)
  , Aggregator1
  , filterWhereExplicit
  , unsafeMakeAggregator
  )
import Rel8.Aggregate.Fold (Fallback (Empty, Fallback))
import Rel8.Expr ( Expr )
import Rel8.Expr.Array (sempty)
import Rel8.Expr.Bool (false, true)
import Rel8.Expr.Eq ((/=.))
import Rel8.Expr.Opaleye
  ( castExpr
  , fromColumn
  , fromPrimExpr
  , toColumn
  , toPrimExpr
  )
import Rel8.Expr.Read (sread)
import Rel8.Expr.Show (show)
import qualified Rel8.Expr.Text as Text
import Rel8.Schema.Null ( Sql, Unnullify )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Array (arrayTypeName, encodeArrayElement)
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information (TypeInformation)
import Rel8.Type.Num ( DBNum )
import Rel8.Type.Ord ( DBMax, DBMin )
import Rel8.Type.String ( DBString )
import Rel8.Type.Sum ( DBSum )


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
count :: Aggregator' fold (Expr a) (Expr Int64)
count =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    (Fallback 0)
    Opaleye.count


-- | Applies 'count' to the column selected by the given function.
countOn :: (i -> Expr a) -> Aggregator' fold i (Expr Int64)
countOn f = lmap f count


-- | Count the number of distinct occurrences of a single column. Corresponds to
-- @COUNT(DISTINCT a)@
countDistinct :: Sql DBEq a
  => Aggregator' fold (Expr a) (Expr Int64)
countDistinct = distinctAggregate count


-- | Applies 'countDistinct' to the column selected by the given function.
countDistinctOn :: Sql DBEq a
  => (i -> Expr a) -> Aggregator' fold i (Expr Int64)
countDistinctOn f = lmap f countDistinct


-- | Corresponds to @COUNT(*)@.
countStar :: Aggregator' fold i (Expr Int64)
countStar = lmap (const true) count


-- | A count of the number of times a given expression is @true@.
countWhere :: Aggregator' fold (Expr Bool) (Expr Int64)
countWhere = filterWhereExplicit ifPP id countStar


-- | Applies 'countWhere' to the column selected by the given function.
countWhereOn :: (i -> Expr Bool) -> Aggregator' fold i (Expr Int64)
countWhereOn f = lmap f countWhere


-- | Corresponds to @bool_and@.
and :: Aggregator' fold (Expr Bool) (Expr Bool)
and =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    (Fallback true)
    Opaleye.boolAnd


-- | Applies 'and' to the column selected by the given function.
andOn :: (i -> Expr Bool) -> Aggregator' fold i (Expr Bool)
andOn f = lmap f and


-- | Corresponds to @bool_or@.
or :: Aggregator' fold (Expr Bool) (Expr Bool)
or =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    (Fallback false)
    Opaleye.boolOr


-- | Applies 'or' to the column selected by the given function.
orOn :: (i -> Expr Bool) -> Aggregator' fold i (Expr Bool)
orOn f = lmap f or


-- | Produce an aggregation for @Expr a@ using the @max@ function.
max :: Sql DBMax a => Aggregator1 (Expr a) (Expr a)
max =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    Empty
    Opaleye.unsafeMax


-- | Applies 'max' to the column selected by the given function.
maxOn :: Sql DBMax a => (i -> Expr a) -> Aggregator1 i (Expr a)
maxOn f = lmap f max


-- | Produce an aggregation for @Expr a@ using the @max@ function.
min :: Sql DBMin a => Aggregator1 (Expr a) (Expr a)
min =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    Empty
    Opaleye.unsafeMin


-- | Applies 'min' to the column selected by the given function.
minOn :: Sql DBMin a => (i -> Expr a) -> Aggregator1 i (Expr a)
minOn f = lmap f min


-- | Corresponds to @sum@. Note that in SQL, @sum@ is type changing - for
-- example the @sum@ of @integer@ returns a @bigint@. Rel8 doesn't support
-- this, and will add explicit casts back to the original input type. This can
-- lead to overflows, and if you anticipate very large sums, you should upcast
-- your input.
sum :: (Sql DBNum a, Sql DBSum a) => Aggregator' fold (Expr a) (Expr a)
sum =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    (Fallback 0)
    Opaleye.unsafeSum


-- | Applies 'sum' to the column selected by the given fucntion.
sumOn :: (Sql DBNum a, Sql DBSum a)
  => (i -> Expr a) -> Aggregator' fold i (Expr a)
sumOn f = lmap f sum


-- | 'sumWhere' is a combination of 'Rel8.filterWhere' and 'sumOn'.
sumWhere :: (Sql DBNum a, Sql DBSum a)
  => (i -> Expr Bool) -> (i -> Expr a) -> Aggregator' fold i (Expr a)
sumWhere condition = filterWhereExplicit ifPP condition . sumOn


-- | Corresponds to @avg@. Note that in SQL, @avg@ is type changing - for
-- example, the @avg@ of @integer@ returns a @numeric@. Rel8 doesn't support
-- this, and will add explicit casts back to the original input type. If you
-- need a fractional result on an integral column, you should cast your input
-- to 'Double' or 'Data.Scientific.Scientific' before calling 'avg'.
avg :: Sql DBSum a => Aggregator1 (Expr a) (Expr a)
avg =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    Empty
    Opaleye.unsafeAvg


-- | Applies 'avg' to the column selected by the given fucntion.
avgOn :: Sql DBSum a => (i -> Expr a) -> Aggregator1 i (Expr a)
avgOn f = lmap f avg


-- | Corresponds to @string_agg()@.
stringAgg :: (Sql IsString a, Sql DBString a)
  => Expr a -> Aggregator' fold (Expr a) (Expr a)
stringAgg delimiter =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (castExpr . fromPrimExpr . fromColumn)
    (Fallback "")
    (Opaleye.stringAgg (toColumn (toPrimExpr delimiter)))


-- | Applies 'stringAgg' to the column selected by the given function.
stringAggOn :: (Sql IsString a, Sql DBString a)
  => Expr a -> (i -> Expr a) -> Aggregator' fold i (Expr a)
stringAggOn delimiter f = lmap f (stringAgg delimiter)


-- | Aggregate a value by grouping by it.
groupByExpr :: Sql DBEq a => Aggregator1 (Expr a) (Expr a)
groupByExpr =
  unsafeMakeAggregator
    (toColumn . toPrimExpr)
    (fromPrimExpr . fromColumn)
    Empty
    Opaleye.groupBy


-- | Applies 'groupByExprOn' to the column selected by the given function.
groupByExprOn :: Sql DBEq a => (i -> Expr a) -> Aggregator1 i (Expr a)
groupByExprOn f = lmap f groupByExpr


-- | Collect expressions values as a list.
listAggExpr :: Sql DBType a => Aggregator' fold (Expr a) (Expr [a])
listAggExpr = slistAggExpr typeInformation


-- | Applies 'listAggExpr' to the column selected by the given function.
listAggExprOn :: Sql DBType a => (i -> Expr a) -> Aggregator' fold i (Expr [a])
listAggExprOn f = lmap f listAggExpr


-- | Collect expressions values as a non-empty list.
nonEmptyAggExpr :: Sql DBType a => Aggregator1 (Expr a) (Expr (NonEmpty a))
nonEmptyAggExpr = snonEmptyAggExpr typeInformation


-- | Applies 'nonEmptyAggExpr' to the column selected by the given function.
nonEmptyAggExprOn :: Sql DBType a
  => (i -> Expr a) -> Aggregator1 i (Expr (NonEmpty a))
nonEmptyAggExprOn f = lmap f nonEmptyAggExpr


-- | Concatenate lists into a single list.
listCatExpr :: Sql DBType a => Aggregator' fold (Expr [a]) (Expr [a])
listCatExpr = slistCatExpr typeInformation


-- | Applies 'listCatExpr' to the column selected by the given function.
listCatExprOn :: Sql DBType a
  => (i -> Expr [a]) -> Aggregator' fold i (Expr [a])
listCatExprOn f = lmap f listCatExpr


-- | Concatenate non-empty lists into a single non-empty list.
nonEmptyCatExpr :: Sql DBType a
  => Aggregator1 (Expr (NonEmpty a)) (Expr (NonEmpty a))
nonEmptyCatExpr = snonEmptyCatExpr typeInformation


-- | Applies 'nonEmptyCatExpr' to the column selected by the given function.
nonEmptyCatExprOn :: Sql DBType a
  => (i -> Expr (NonEmpty a)) -> Aggregator1 i (Expr (NonEmpty a))
nonEmptyCatExprOn f = lmap f nonEmptyCatExpr


-- | 'distinctAggregate' modifies an 'Aggregator' to consider only distinct
-- values of a particular column.
distinctAggregate :: Sql DBEq a
  => Aggregator' fold i (Expr a) -> Aggregator' fold i (Expr a)
distinctAggregate (Aggregator fallback a) =
  Aggregator fallback (Opaleye.distinctAggregator a)


slistAggExpr :: ()
  => TypeInformation (Unnullify a) -> Aggregator' fold (Expr a) (Expr [a])
slistAggExpr info =
  unsafeMakeAggregator
    (toColumn . encodeArrayElement info . toPrimExpr)
    (fromPrimExpr . fromColumn)
    (Fallback (sempty info))
    Opaleye.arrayAgg


snonEmptyAggExpr :: ()
  => TypeInformation (Unnullify a) -> Aggregator1 (Expr a) (Expr (NonEmpty a))
snonEmptyAggExpr info =
  unsafeMakeAggregator
    (toColumn . encodeArrayElement info . toPrimExpr)
    (fromPrimExpr . fromColumn)
    Empty
    Opaleye.arrayAgg


slistCatExpr :: ()
  => TypeInformation (Unnullify a) -> Aggregator' fold (Expr [a]) (Expr [a])
slistCatExpr info = dimap (unbracket . show) (sread name . bracket) agg
  where
    bracket a = "{" <> a <> "}"
    unbracket a = Text.substr a 2 (Just (Text.length a - 2))
    agg = filterWhereExplicit ifPP (/=. "") (stringAgg ",")
    name = arrayTypeName info


snonEmptyCatExpr :: ()
  => TypeInformation (Unnullify a)
  -> Aggregator1 (Expr (NonEmpty a)) (Expr (NonEmpty a))
snonEmptyCatExpr info = dimap (unbracket . show) (sread name . bracket) agg
  where
    bracket a = "{" <> a <> "}"
    unbracket a = Text.substr a 2 (Just (Text.length a - 2))
    agg = filterWhereExplicit ifPP (/=. "") (stringAgg ",")
    name = arrayTypeName info


ifPP :: Opaleye.IfPP (Expr a) (Expr a)
ifPP = dimap from to Opaleye.ifPPField
  where
    from = toColumn . toPrimExpr
    to = fromPrimExpr . fromColumn
