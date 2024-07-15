{-# language FlexibleContexts #-}

module Rel8.Table.Aggregate.Maybe
  ( filterWhereOptional
  , optionalAggregate
  , aggregateJustTable
  , aggregateJustTable1
  , aggregateMaybeTable
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye

-- profunctors
import Data.Profunctor (lmap)

-- rel8
import Rel8.Aggregate
  ( Aggregator' (Aggregator)
  , Aggregator, toAggregator
  , Aggregator1, toAggregator1
  )
import Rel8.Aggregate.Fold (Fallback (Fallback))
import Rel8.Expr (Expr)
import Rel8.Expr.Aggregate (groupByExprOn)
import Rel8.Expr.Opaleye (toColumn, toPrimExpr)
import Rel8.Table (Table)
import Rel8.Table.Aggregate (filterWhere)
import Rel8.Table.Maybe
  ( MaybeTable (MaybeTable, just, tag), justTable, nothingTable
  , isJustTable
  , makeMaybeTable
  )
import Rel8.Table.Nullify (aggregateNullify, unsafeUnnullifyTable)


-- | A variant of 'filterWhere' that can be used with an 'Aggregator1'
-- (upgrading it to an 'Aggregator' in the process). It returns
-- 'nothingTable' in the case where the predicate matches zero rows.
filterWhereOptional :: Table Expr a
  => (i -> Expr Bool) -> Aggregator' fold i a -> Aggregator' fold' i (MaybeTable Expr a)
filterWhereOptional f (Aggregator _ aggregator) =
  Aggregator (Fallback nothingTable) $
    Opaleye.filterWhereInternal makeMaybeTable (toColumn . toPrimExpr . f) aggregator


-- | 'optionalAggregate' upgrades an 'Aggregator1' into an 'Aggregator' by
-- having it return 'nothingTable' when aggregating over an empty collection
-- of rows.
optionalAggregate :: Table Expr a
  => Aggregator' fold i a -> Aggregator' fold' i (MaybeTable Expr a)
optionalAggregate = toAggregator nothingTable . fmap justTable


-- | Lift an 'Aggregator' to operate on a 'MaybeTable'. If the input query has
-- @'justTable' i@s, they are folded into a single @a@ by the given aggregator
-- — in the case where the input query is all 'nothingTable's, the
-- 'Aggregator'\'s fallback @a@ is returned.
aggregateJustTable :: Table Expr a
  => Aggregator i a
  -> Aggregator' fold (MaybeTable Expr i) a
aggregateJustTable =
  filterWhere isJustTable . lmap (unsafeUnnullifyTable . just)


-- | Lift an 'Aggregator1' to operate on a 'MaybeTable'. If the input query
-- has @'justTable' i@s, they are folded into a single @'justTable' a@ by the
-- given aggregator — in the case where the input query is all
-- 'nothingTable's, a single 'nothingTable' row is returned.
aggregateJustTable1 :: Table Expr a
  => Aggregator' fold i a
  -> Aggregator' fold' (MaybeTable Expr i) (MaybeTable Expr a)
aggregateJustTable1 =
  filterWhereOptional isJustTable . lmap (unsafeUnnullifyTable . just)


-- | Lift an aggregator to operate on a 'MaybeTable'. @nothingTable@s and
-- @justTable@s are grouped separately.
aggregateMaybeTable :: ()
  => Aggregator' fold i a
  -> Aggregator1 (MaybeTable Expr i) (MaybeTable Expr a)
aggregateMaybeTable aggregator =
  MaybeTable
    <$> groupByExprOn tag
    <*> lmap just (toAggregator1 (aggregateNullify aggregator))
