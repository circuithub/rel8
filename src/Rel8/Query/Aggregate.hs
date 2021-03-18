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
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Map ( MapTable )
import Rel8.Table.Opaleye ( aggregator )
import Rel8.Schema.Context ( Aggregation, DB )


aggregate :: MapTable Aggregation DB aggregates exprs
  => Query aggregates -> Query exprs
aggregate = mapOpaleye (Opaleye.aggregate aggregator)
