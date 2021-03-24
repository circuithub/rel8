{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.Update
  ( Update(..)
  , update
  )
where

-- base
import Control.Exception ( throwIO )
import Data.Kind ( Type )
import Prelude

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye

-- profunctors
import Data.Profunctor ( lmap )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( toColumn, toPrimExpr )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Returning ( Returning( Projection, NumberOfRowsAffected ) )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Table.Insert ( toInsert )
import Rel8.Table.Opaleye ( table, unpackspec )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of an @UPDATE@ statement.
type Update :: Type -> Type
data Update a where
  Update :: Selects names exprs =>
    { target :: TableSchema names
      -- ^ Which table to update.
    , set :: exprs -> exprs
      -- ^ How to update each selected row.
    , updateWhere :: exprs -> Expr Bool
      -- ^ Which rows to select for update.
    , returning :: Returning names a
      -- ^ What to return from the @UPDATE@ statement.
    }
    -> Update a


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
update :: Connection -> Update a -> IO a
update c Update {target, set, updateWhere, returning} =
  case returning of
    NumberOfRowsAffected -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = Hasql.rowsAffected
        prepare = False
        sql = Opaleye.arrangeUpdateSql target' set' where'
          where
            target' = lmap toInsert $ table $ toColumns <$> target
            set' = toColumns . set . fromColumns
            where' = toColumn . toPrimExpr . updateWhere . fromColumns

    Projection project -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decoder project
        prepare = False
        sql =
          Opaleye.arrangeUpdateReturningSql
            unpackspec
            target'
            set'
            where'
            project'
          where
            target' = lmap toInsert $ table $ toColumns <$> target
            set' = toColumns . set . fromColumns
            where' = toColumn . toPrimExpr . updateWhere . fromColumns
            project' = toColumns . project . fromColumns

  where
    decoder :: forall exprs projection a. Serializable projection a
      => (exprs -> projection) -> Hasql.Result [a]
    decoder _ = Hasql.rowList (parse @projection @a)
