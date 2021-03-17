{-# language BlockArguments #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Delete ( Delete(..), delete ) where

-- base
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( MonadIO( liftIO ) )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye

-- rel8
import Rel8.Expr ( Expr( toPrimExpr ), column )
import Rel8.Query ( hasqlRowDecoder, unpackspec )
import Rel8.Returning ( Returning( NumberOfRowsAffected, Projection ) )
import Rel8.Serializable ( Serializable )
import Rel8.Table.Congruent ( mapTable )
import Rel8.Table.Selects ( Selects )
import Rel8.TableSchema ( TableSchema, ddlTable, writer )
import Rel8.TableSchema.ColumnSchema ( ColumnSchema( columnName ) )

-- text
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of a @DELETE@ statement.
data Delete from return where
  Delete
    :: Selects from row
    => { from :: TableSchema from
         -- ^ Which table to delete from.
       , deleteWhere :: row -> Expr Bool
         -- ^ Which rows should be selected for deletion.
       , returning :: Returning from return
         -- ^ What to return from the @DELETE@ statement.
       }
    -> Delete from return


-- | Run a @DELETE@ statement.
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
--
-- >>> :{
-- delete c Delete
--   { from = projectSchema
--   , deleteWhere = \p -> projectName p ==. lit "rel8"
--   , returning = Projection projectName
--   }
-- :}
-- ["rel8"]
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
delete :: MonadIO m => Connection -> Delete from returning -> m returning
delete conn Delete{ from = deleteFrom, deleteWhere, returning } = liftIO
  case returning of
    NumberOfRowsAffected -> Hasql.run session conn >>= either throwIO return where
      session = Hasql.statement () statement where
        statement = Hasql.Statement q Hasql.noParams Hasql.rowsAffected False where
          q = encodeUtf8 $ pack $ Opaleye.arrangeDeleteSql table f where
            f = Opaleye.Column . toPrimExpr . deleteWhere . mapTable (column . columnName)
            table = ddlTable deleteFrom (writer deleteFrom)

    Projection p -> Hasql.run session conn >>= either throwIO return where
      session = Hasql.statement () statement where
        statement = Hasql.Statement q Hasql.noParams (mkDecoder p) False where
          q = encodeUtf8 $ pack $ Opaleye.arrangeDeleteReturningSql unpackspec table f g where
            f = Opaleye.Column . toPrimExpr . deleteWhere . mapTable (column . columnName)
            table = ddlTable deleteFrom (writer deleteFrom)
            g = p . mapTable (column . columnName)

          mkDecoder :: forall row projection a. Serializable projection a => (row -> projection) -> Hasql.Result [a]
          mkDecoder _ = Hasql.rowList (hasqlRowDecoder @projection)
