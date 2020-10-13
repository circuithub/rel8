{-# language DeriveFunctor #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}

{-# options -fno-warn-deprecations #-}

module Rel8.TableSchema where

import Data.Kind
import qualified Opaleye.Internal.Table as Opaleye


{-| The schema for a table. This is used to specify the name and schema
that a table belongs to (the @FROM@ part of a SQL query), along with
the schema of the columns within this table.

For each selectable table in your database, you should provide a @TableSchema@
in order to interact with the table via Rel8. For a table storing a list of
Haskell packages (as defined in the example for 'Rel8.Column.Column'), we would
write:

@
haskellPackage :: TableSchema ( HaskellPackage 'Rel8.ColumnSchema.ColumnSchema' )
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

data TableSchema ( schema :: Type ) =
  TableSchema
    { tableName :: String
      -- ^ The name of the table.
    , tableSchema :: Maybe String
      -- ^ The schema that this table belongs to. If 'Nothing', whatever is on
      -- the connection's @search_path@ will be used.
    , tableColumns :: schema
      -- ^ The columns of the table. Typically you would use a a higher-kinded
      -- data type here, parameterized by the 'Rel8.ColumnSchema.ColumnSchema' functor.
    }
  deriving
    ( Functor )


toOpaleyeTable
  :: TableSchema schema
  -> Opaleye.Writer write view
  -> Opaleye.View view
  -> Opaleye.Table write view
toOpaleyeTable TableSchema{ tableName, tableSchema } writer view =
  case tableSchema of
    Nothing ->
      Opaleye.Table tableName tableFields

    Just s ->
      Opaleye.TableWithSchema s tableName tableFields

  where

    tableFields =
      Opaleye.TableFields writer view
