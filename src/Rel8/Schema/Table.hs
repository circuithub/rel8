{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.Schema.Table (
  TableSchema (..),
  ppTable,
)
where

-- base
import Data.Kind (Type)
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye

-- pretty
import Text.PrettyPrint (Doc)


{- | The schema for a table. This is used to specify the name and schema that a
table belongs to (the @FROM@ part of a SQL query), along with the schema of
the columns within this table.

For each selectable table in your database, you should provide a
@TableSchema@ in order to interact with the table via Rel8.
-}
type TableSchema :: Type -> Type
data TableSchema names = TableSchema
  { name :: String
  -- ^ The name of the table.
  , schema :: Maybe String
  -- ^ The schema that this table belongs to. If 'Nothing', whatever is on
  -- the connection's @search_path@ will be used.
  , columns :: names
  -- ^ The columns of the table. Typically you would use a a higher-kinded
  -- data type here, parameterized by the 'Rel8.ColumnSchema.ColumnSchema' functor.
  }
  deriving stock (Functor)


ppTable :: TableSchema a -> Doc
ppTable TableSchema{name, schema} =
  Opaleye.ppTable
    Opaleye.SqlTable
      { sqlTableSchemaName = schema
      , sqlTableName = name
      }
