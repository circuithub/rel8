{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.Delete
  ( Delete(..)
  , delete
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

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( toColumn, toPrimExpr )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Returning ( Returning( NumberOfRowsAffected, Projection ) )
import Rel8.Table.Cols ( fromCols, toCols )
import Rel8.Table.Opaleye ( castTable, table, unpackspec )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of a @DELETE@ statement.
type Delete :: Type -> Type
data Delete a where
  Delete :: Selects names exprs =>
    { from :: TableSchema names
      -- ^ Which table to delete from.
    , deleteWhere :: exprs -> Expr Bool
      -- ^ Which rows should be selected for deletion.
    , returning :: Returning names a
      -- ^ What to return from the @DELETE@ statement.
    }
    -> Delete a


-- | Run a @DELETE@ statement.
delete :: Connection -> Delete a -> IO a
delete c Delete {from, deleteWhere, returning} =
  case returning of
    NumberOfRowsAffected -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = Hasql.rowsAffected
        prepare = False
        sql = Opaleye.arrangeDeleteSql from' where'
          where
            from' = table $ toCols <$> from
            where' = toColumn . toPrimExpr . deleteWhere . fromCols

    Projection project -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decoder project
        prepare = False
        sql =
          Opaleye.arrangeDeleteReturningSql unpackspec from' where' project'
          where
            from' = table $ toCols <$> from
            where' = toColumn . toPrimExpr . deleteWhere . fromCols
            project' = castTable . toCols . project . fromCols
  where
    decoder :: forall exprs projection a. Serializable projection a
      => (exprs -> projection) -> Hasql.Result [a]
    decoder _ = Hasql.rowList (parse @projection @a)
