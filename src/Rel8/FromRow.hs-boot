{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language UndecidableSuperClasses #-}

module Rel8.FromRow where

import Database.PostgreSQL.Simple.FromRow
import Rel8.ZipLeaves
import Rel8.Expr
import {-# source #-} Rel8.Query

class ZipLeaves sql sql ( Expr Query ) ( Expr Query ) => FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell
