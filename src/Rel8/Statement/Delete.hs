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
import Control.Monad ( (>=>) )
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
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Returning ( Returning( NumberOfRowsAffected, Projection ) )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Table.Opaleye ( table, unpackspec )
import Rel8.Table.Recontextualize ( Selects )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


type Delete :: Type -> Type
data Delete a where
  Delete :: Selects names exprs =>
    { from :: TableSchema names
    , deleteWhere :: exprs -> Expr nullability Bool
    , returning :: Returning names a
    }
    -> Delete a


delete :: Delete a -> Connection -> IO a
delete Delete {from, deleteWhere, returning} =
  case returning of
    NumberOfRowsAffected -> Hasql.run session >=> either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = Hasql.rowsAffected
        prepare = False
        sql = Opaleye.arrangeDeleteSql from' where'
          where
            from' = table $ toColumns <$> from
            where' = toColumn . toPrimExpr . deleteWhere . fromColumns

    Projection project -> Hasql.run session >=> either throwIO pure
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
            from' = table $ toColumns <$> from
            where' = toColumn . toPrimExpr . deleteWhere . fromColumns
            project' = toColumns . project . fromColumns
  where
    decoder :: forall exprs projection a. Serializable projection a
      => (exprs -> projection) -> Hasql.Result [a]
    decoder _ = Hasql.rowList (parse @a @projection)
