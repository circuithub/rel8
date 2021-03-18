{-# language BlockArguments #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Statement.Update ( Update(..), update ) where

-- base
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( MonadIO( liftIO ) )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import Rel8.Expr ( Expr( toPrimExpr ), column )
import Rel8.Serializable ( Serializable, hasqlRowDecoder )
import Rel8.Statement.Returning ( Returning( Projection, NumberOfRowsAffected ) )
import Rel8.Table.Congruent ( mapTable )
import Rel8.Table.Opaleye ( unpackspec )
import Rel8.Table.Selects ( Selects )
import Rel8.TableSchema ( TableSchema, ddlTable, writer )
import Rel8.TableSchema.ColumnSchema ( ColumnSchema( columnName ) )

-- text
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of an @UPDATE@ statement.
data Update target returning where
  Update
    :: Selects target row
    => { target :: TableSchema target
         -- ^ Which table to update.
       , set :: row -> row
         -- ^ How to update each selected row.
       , updateWhere :: row -> Expr Bool
         -- ^ Which rows to select for update.
       , returning :: Returning target returning
         -- ^ What to return from the @UPDATE@ statement.
       }
    -> Update target returning


-- | Run an @UPDATE@ statement.
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
--
-- >>> :{
-- update c Update
--   { target = projectSchema
--   , set = \p -> p { projectName = "Rel8!" }
--   , updateWhere = \p -> projectName p ==. lit "rel8"
--   , returning = NumberOfRowsAffected
--   }
-- :}
-- 1
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
-- Project {projectAuthorId = 1, projectName = "Rel8!"}
update :: MonadIO m => Connection -> Update target returning -> m returning
update conn Update{ target, set, updateWhere, returning } = liftIO
  case returning of
    NumberOfRowsAffected -> Hasql.run session conn >>= either throwIO return where
      session = Hasql.statement () statement where
        statement = Hasql.Statement q Hasql.noParams Hasql.rowsAffected False where
          q = encodeUtf8 $ pack $ Opaleye.arrangeUpdateSql table g f where
            f = Opaleye.Column . toPrimExpr . updateWhere . mapTable (column . columnName)
            g = set . mapTable (column . columnName)

    Projection p -> Hasql.run session conn >>= either throwIO return where
      session = Hasql.statement () statement where
        statement = Hasql.Statement q Hasql.noParams (mkDecoder p) False where
          q = encodeUtf8 $ pack $ Opaleye.arrangeUpdateReturningSql unpackspec table g f h where
            f = Opaleye.Column . toPrimExpr . updateWhere . mapTable (column . columnName)
            g = set . mapTable (column . columnName)
            h = p . mapTable (column . columnName)

          mkDecoder :: forall row projection a. Serializable projection a => (row -> projection) -> Hasql.Result [a]
          mkDecoder _ = Hasql.rowList (hasqlRowDecoder @projection)
  where
    table = ddlTable target (writer target)
