{-# LANGUAGE OverloadedStrings #-}

module Rel8.Internal.Aggregate.Range (
  rangeAgg,  
) where

-- base
import Prelude

-- rel8
import Rel8.Internal.Aggregate (Aggregator', toAggregator)
import Rel8.Internal.Aggregate.Function (aggregateFunction)
import Rel8.Internal.Data.Range (Multirange, Range)
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Type.Range (DBRange)


rangeAgg ::
  DBRange a =>
  Aggregator' fold (Expr (Range a)) (Expr (Multirange a))
rangeAgg = toAggregator mempty $ aggregateFunction "range_agg"
