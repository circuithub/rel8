{-# LANGUAGE OverloadedStrings #-}

module Rel8.Aggregate.Range (
  rangeAgg,  
) where

-- base
import Prelude

-- rel8
import Rel8.Aggregate (Aggregator', toAggregator)
import Rel8.Aggregate.Function (aggregateFunction)
import Rel8.Data.Range (Multirange, Range)
import Rel8.Expr (Expr)
import Rel8.Type.Range (DBRange)


rangeAgg ::
  DBRange a =>
  Aggregator' fold (Expr (Range a)) (Expr (Multirange a))
rangeAgg = toAggregator mempty $ aggregateFunction "range_agg"
