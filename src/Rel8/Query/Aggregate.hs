{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Query.Aggregate
  ( aggregate
  , countRows
  , mode
  )
where

-- base
import Data.Functor.Contravariant ( (>$<) )
import Data.Int ( Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Aggregate as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregates )
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( countStar )
import Rel8.Expr.Order ( desc )
import Rel8.Query ( Query )
import Rel8.Query.Limit ( limit )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Query.Order ( orderBy )
import Rel8.Table ( toColumns )
import Rel8.Table.Aggregate ( hgroupBy )
import Rel8.Table.Cols ( Cols( Cols ), fromCols )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Opaleye ( aggregator )
import Rel8.Table.Maybe ( maybeTable )


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: Aggregates aggregates exprs => Query aggregates -> Query exprs
aggregate = mapOpaleye (Opaleye.aggregate aggregator)


-- | Count the number of rows returned by a query. Note that this is different
-- from @countStar@, as even if the given query yields no rows, @countRows@
-- will return @0@.
countRows :: Query a -> Query (Expr Int64)
countRows = fmap (maybeTable 0 id) . optional . aggregate . fmap (const countStar)


-- | Return the most common row in a query.
mode :: forall a. EqTable a => Query a -> Query a
mode rows = limit 1 $ fmap (fromCols . snd) $ orderBy (fst >$< desc) $ do
  aggregate $ do
    row <- toColumns <$> rows
    pure (countStar, Cols $ hgroupBy (eqTable @a) row)
