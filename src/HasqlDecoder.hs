module HasqlDecoder where

data HasqlDecoder a where
  DecodeNotNull :: Hasql.Value x -> (x -> a) -> HasqlDecoder a
  DecodeNull :: Hasql.Value x -> (Maybe x -> Either String a) -> HasqlDecoder a
