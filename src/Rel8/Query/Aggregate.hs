{-# language FlexibleContexts #-}

module Rel8.Query.Aggregate
  ( aggregate
  , countRows
  )
where

-- base
import Data.Int ( Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Aggregate as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregator(..) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( countStar )
import Rel8.Query ( Query )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Maybe ( maybeTable )


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: Aggregator i o -> Query i -> Query o
aggregate (Aggregator aggregator) = mapOpaleye (Opaleye.aggregate aggregator)


-- | Count the number of rows returned by a query. Note that this is different
-- from @countStar@, as even if the given query yields no rows, @countRows@
-- will return @0@.
countRows :: Query a -> Query (Expr Int64)
countRows = fmap (maybeTable 0 id) . optional . aggregate countStar
