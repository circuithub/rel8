{-# language DeriveFunctor #-}

module Rel8.TableSchema ( TableSchema(..) ) where


{-| The schema for a table. This is used to specify the name and schema
that a table belongs to (the @FROM@ part of a SQL query), along with
the schema of the columns within this table.

For each selectable table in your database, you should provide a @TableSchema@
in order to interact with the table via Rel8. For a table storing a list of
Haskell packages (as defined in the example for 'Column'), we would write:

@
haskellPackage :: TableSchema ( HaskellPackage 'ColumnSchema' )
haskellPackage =
  TableSchema
    { tableName = "haskell_package"
    , tableSchema = Nothing -- Assumes that haskell_package is reachable from your connections search_path
    , tableColumns =
        HaskellPackage { packageName = "name"
                       , packageAuthor = "author"
                       }
    }
@
-}

data TableSchema schema =
  TableSchema
    { tableName :: String
      -- ^ The name of the table.
    , tableSchema :: Maybe String
      -- ^ The schema that this table belongs to. If 'Nothing', whatever is on
      -- the connection's @search_path@ will be used.
    , tableColumns :: schema
      -- ^ The columns of the table. Typically you would use a a higher-kinded
      -- data type here, parameterized by the 'ColumnSchema' functor.
    }
  deriving
    ( Functor )
