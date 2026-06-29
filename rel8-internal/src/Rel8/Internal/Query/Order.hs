module Rel8.Internal.Query.Order
  ( orderBy
  )
where

-- base
import Prelude ()

-- opaleye
import qualified Opaleye.Order as Opaleye ( orderBy )

-- rel8
import Rel8.Internal.Order ( Order( Order ) )
import Rel8.Internal.Query ( Query )
import Rel8.Internal.Query.Opaleye ( mapOpaleye )


-- | Order the rows returned by a query.
orderBy :: Order a -> Query a -> Query a
orderBy (Order o) = mapOpaleye (Opaleye.orderBy o)
