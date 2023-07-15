{-# language LambdaCase #-}

module Rel8.Schema.Escape
  ( escape
  )
where

-- base
import Prelude

-- pretty
import Text.PrettyPrint (Doc, doubleQuotes, text)


escape :: String -> Doc
escape = doubleQuotes . text . concatMap go
  where
    go = \case
      '"' -> "\"\""
      c -> [c]
