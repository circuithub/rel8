module Rel8.Query.Locking
  ( forUpdate
  )
where

-- opaleye
import qualified Opaleye.Internal.Locking as Opaleye

-- rel8
import Rel8.Query (Query)
import Rel8.Query.Opaleye (mapOpaleye)


-- | Adds a PostgreSQL @FOR UPDATE@ locking clause to a query.
forUpdate :: Query a -> Query a
forUpdate = mapOpaleye Opaleye.forUpdate
