module Rel8.Type.Parser
  ( parse
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString as A

-- base
import Prelude

-- bytestring
import Data.ByteString (ByteString)


parse :: A.Parser a -> ByteString -> Either String a
parse parser = A.parseOnly (parser <* A.endOfInput)
