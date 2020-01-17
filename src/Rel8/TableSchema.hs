{-# language DeriveFunctor #-}

module Rel8.TableSchema ( TableSchema(..) ) where


-- | The schema for a table.
data TableSchema schema =
  TableSchema
    { tableName :: String
      -- ^ The name of the table.
    , tableSchema :: Maybe String
      -- ^ The schema that this table belongs to. If 'Nothing', whatever is on
      -- the connection's @search_path@ will be used.
    , tableColumns :: schema
      -- ^ The columns of the table.
    }
  deriving
    ( Functor )
