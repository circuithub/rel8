{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.TableSchema ( TableSchema(..), toOpaleyeTable, ddlTable, writer ) where

-- base
import Control.Monad ( void )
import Data.Functor.Compose ( Compose( Compose, getCompose ) )
import Data.Kind ( Type )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Table as Opaleye

-- rel8
import Rel8.Expr ( Expr( toPrimExpr ), column )
import Rel8.HTable ( HTable( hfield, htraverse, htabulate ) )
import Rel8.Table ( Table( toColumns, Columns ) )
import Rel8.Table.Selects ( Selects )
import Rel8.TableSchema.ColumnSchema ( ColumnSchema( ColumnSchema, columnName ) )


-- | The schema for a table. This is used to specify the name and schema that a
-- table belongs to (the @FROM@ part of a SQL query), along with the schema of
-- the columns within this table.
-- 
-- For each selectable table in your database, you should provide a
-- @TableSchema@ in order to interact with the table via Rel8. For a table
-- storing a list of projects (as defined in the introduction):
-- 
-- >>> :{
-- projectSchema :: TableSchema (Project ColumnSchema)
-- projectSchema = TableSchema
--   { tableName = "project"
--   , tableSchema = Nothing -- Assumes that the 'project' table is reachable from your connection's search_path
--   , tableColumns = Project 
--       { projectAuthorId = "author_id"
--       , projectName = "name"
--       }
--   }
-- :}
data TableSchema (schema :: Type) = TableSchema
  { tableName :: String
    -- ^ The name of the table.
  , tableSchema :: Maybe String
    -- ^ The schema that this table belongs to. If 'Nothing', whatever is on
    -- the connection's @search_path@ will be used.
  , tableColumns :: schema
    -- ^ The columns of the table. Typically you would use a a higher-kinded
    -- data type here, parameterized by the 'Rel8.ColumnSchema.ColumnSchema' functor.
  } deriving stock Functor


toOpaleyeTable
  :: TableSchema schema
  -> Opaleye.Writer write view
  -> Opaleye.View view
  -> Opaleye.Table write view
toOpaleyeTable TableSchema{ tableName, tableSchema } writer_ view =
  maybe withoutSchema withSchema tableSchema
  where
    tableFields = Opaleye.TableFields writer_ view

    withoutSchema = Opaleye.Table tableName tableFields
    withSchema s = Opaleye.TableWithSchema s tableName tableFields


ddlTable :: TableSchema schema -> Opaleye.Writer value schema -> Opaleye.Table value schema
ddlTable schema writer_ =
  toOpaleyeTable schema writer_ $ Opaleye.View (tableColumns schema)


writer
  :: forall value schema
   . Selects schema value
  => TableSchema schema -> Opaleye.Writer value schema
writer into_ =
  let
    go
      :: forall f list
       . ( Functor list, Applicative f )
      => ( ( list Opaleye.PrimExpr, String ) -> f () )
      -> list value
      -> f ()
    go f xs =
      void $
        htraverse @(Columns schema) @_ @Expr getCompose $
          htabulate @(Columns schema) @(Compose f Expr) \i ->
            case hfield (toColumns (tableColumns into_)) i of
              ColumnSchema{ columnName } ->
                Compose $
                  column columnName <$
                  f ( toPrimExpr . flip hfield i . toColumns <$> xs
                    , columnName
                    )

  in
  Opaleye.Writer ( Opaleye.PackMap go )
