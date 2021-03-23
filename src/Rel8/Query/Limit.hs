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


-- | @limit n@ select at most @n@ rows from a query.  @limit n@ is equivalent
-- to the SQL @LIMIT n@.
--
-- >>> select c $ limit 3 $ values [ lit x | x <- [ 1..5 :: Int32 ] ]
-- [1,2,3]
limit :: Word -> Query a -> Query a
limit = mapOpaleye . Opaleye.limit . fromIntegral


-- | @offset n@ drops the first @n@ rows from a query. @offset n@ is equivalent
-- to the SQL @OFFSET n@.
--
-- >>> select c $ offset 3 $ values [ lit x | x <- [ 1..5 :: Int32 ] ]
-- [4,5]
offset :: Word -> Query a -> Query a
offset = mapOpaleye . Opaleye.offset . fromIntegral
