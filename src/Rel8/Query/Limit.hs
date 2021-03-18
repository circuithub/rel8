module Rel8.Query.Limit
  ( limit
  , offset
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )


limit :: Word -> Query a -> Query a
limit = mapOpaleye . Opaleye.limit . fromIntegral


offset :: Word -> Query a -> Query a
offset = mapOpaleye . Opaleye.offset . fromIntegral
