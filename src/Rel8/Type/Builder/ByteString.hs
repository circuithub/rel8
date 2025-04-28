{-# language OverloadedStrings #-}

module Rel8.Type.Builder.ByteString (
  bytestring,
) where

-- base
import Prelude

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteStringHex, string7)


bytestring :: ByteString -> Builder
bytestring bytes = string7 "\\x" <> byteStringHex bytes
