module Rel8.Query.Order (
  orderBy,
)
where

-- base
import Prelude ()

-- opaleye
import qualified Opaleye.Order as Opaleye (orderBy)

-- rel8
import Rel8.Order (Order (Order))
import Rel8.Query (Query)
import Rel8.Query.Opaleye (mapOpaleye)


-- | Order the rows returned by a query.
orderBy :: Order a -> Query a -> Query a
orderBy (Order o) = mapOpaleye (Opaleye.orderBy o)
