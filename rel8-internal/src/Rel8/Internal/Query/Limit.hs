module Rel8.Internal.Query.Limit
  ( limit
  , offset
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye

-- rel8
import Rel8.Internal.Query ( Query )
import Rel8.Internal.Query.Opaleye ( mapOpaleye )


-- | @limit n@ select at most @n@ rows from a query.  @limit n@ is equivalent
-- to the SQL @LIMIT n@.
limit :: Word -> Query a -> Query a
limit = mapOpaleye . Opaleye.limit . fromIntegral


-- | @offset n@ drops the first @n@ rows from a query. @offset n@ is equivalent
-- to the SQL @OFFSET n@.
offset :: Word -> Query a -> Query a
offset = mapOpaleye . Opaleye.offset . fromIntegral
