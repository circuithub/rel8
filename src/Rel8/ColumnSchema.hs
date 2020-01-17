{-# language GeneralizedNewtypeDeriving #-}

module Rel8.ColumnSchema ( ColumnSchema(..) ) where

import Data.String ( IsString )


-- | The schema for a column in a table.
newtype ColumnSchema a =
  ColumnSchema { columnName :: String }
  deriving ( IsString )
