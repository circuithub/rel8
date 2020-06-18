{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language UndecidableSuperClasses #-}

module Rel8.FromRow where

import Database.PostgreSQL.Simple.FromRow
import Rel8.Expr

class ExprTable sql => FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell
