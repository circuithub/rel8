{-# language FunctionalDependencies #-}

module Rel8.FromRow where

import Database.PostgreSQL.Simple.FromRow

class FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell
