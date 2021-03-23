{-# language FlexibleContexts #-}

module Rel8.Query.Aggregate
  ( aggregate
  )
where

-- base
import Prelude ()

-- opaleye
import qualified Opaleye.Aggregate as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Opaleye ( aggregator )


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: Query (Aggregate exprs) -> Query exprs
aggregate = mapOpaleye (Opaleye.aggregate aggregator)
