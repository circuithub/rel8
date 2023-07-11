{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Schema.Table
  ( TableSchema(..)
  , ppTable
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- pretty
import Text.PrettyPrint ( Doc )

-- rel8
import Rel8.Schema.QualifiedName (QualifiedName, ppQualifiedName)


-- | The schema for a table. This is used to specify the name and schema that a
-- table belongs to (the @FROM@ part of a SQL query), along with the schema of
-- the columns within this table.
-- 
-- For each selectable table in your database, you should provide a
-- @TableSchema@ in order to interact with the table via Rel8.
type TableSchema :: Type -> Type
data TableSchema names = TableSchema
  { name :: QualifiedName
    -- ^ The name of the table.
  , columns :: names
    -- ^ The columns of the table. Typically you would use a 'Rel8.Rel8able'
    -- data type here, parameterized by the 'Rel8.Name' context.
  }
  deriving stock Functor


ppTable :: TableSchema a -> Doc
ppTable TableSchema {name} = ppQualifiedName name
