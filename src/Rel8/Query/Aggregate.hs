{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}

module Rel8.Query.Aggregate
  ( aggregate
  , aggregate1
  , aggregateU
  , countRows
  )
where

-- base
import Control.Applicative (liftA2)
import Data.Int ( Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Adaptors as Opaleye
import qualified Opaleye.Aggregate as Opaleye

-- rel8
import Rel8.Aggregate (Aggregator' (Aggregator), Aggregator)
import Rel8.Aggregate.Fold (Fallback (Fallback))
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( countStar )
import Rel8.Expr.Bool (true)
import Rel8.Query ( Query )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table (Table)
import Rel8.Table.Maybe (fromMaybeTable)
import Rel8.Table.Opaleye (unpackspec)


-- | Apply an 'Aggregator' to all rows returned by a 'Query'. If the 'Query'
-- is empty, then a single \"fallback\" row is returned, composed of the
-- identity elements of the constituent aggregation functions.
aggregate :: (Table Expr i, Table Expr a) => Aggregator i a -> Query i -> Query a
aggregate aggregator@(Aggregator (Fallback fallback) _) =
  fmap (fromMaybeTable fallback) . optional . aggregate1 aggregator


-- | Apply an 'Rel8.Aggregator1' to all rows returned by a 'Query'. If
-- the 'Query' is empty, then zero rows are returned.
aggregate1 :: Table Expr i => Aggregator' fold i a -> Query i -> Query a
aggregate1 = aggregateU unpackspec


aggregateU :: Opaleye.Unpackspec i i -> Aggregator' fold i a -> Query i -> Query a
aggregateU unpack (Aggregator _ aggregator) =
  mapOpaleye (Opaleye.aggregateExplicit unpack aggregator)


-- | Count the number of rows returned by a query. Note that this is different
-- from @countStar@, as even if the given query yields no rows, @countRows@
-- will return @0@.
countRows :: Query a -> Query (Expr Int64)
countRows = aggregate countStar . (true <$)
