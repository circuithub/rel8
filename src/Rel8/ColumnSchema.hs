{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}

module Rel8.ColumnSchema ( ColumnSchema(..) ) where

import Data.Kind
import Data.String ( IsString )


{-| The schema for a column in a table. To construct values of this type,
enable the @OverloadedStrings@ language extension and write literal Haskell
strings:

@
\{\-\# LANGUAGE OverloadedStrings -\}
tableSchema :: TableSchema ( HaskellPackage ColumnSchema )
tableSchema =
  TableSchema
    { ...
    , tableColumns =
        HaskallPackage
          { packageName = "name" -- Here "name" :: ColumnSchema due to OverloadedStrings
          }
    }
@

If you want to programatically create @ColumnSchema@'s, you can use
'Data.String.fromString':

@
import Data.String ( fromString )

commonPrefix :: String
commonPrefix = "prefix_"

tableSchema :: TableSchema ( HaskellPackage ColumnSchema )
tableSchema =
  TableSchema
    { ...
    , tableColumns =
        HaskallPackage
          { packageName = fromString ( prefix ++ "name" )
          }
    }
@

-}
newtype ColumnSchema ( a :: Type ) =
  ColumnSchema { columnName :: String }
  deriving ( IsString )
