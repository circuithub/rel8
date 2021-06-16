{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.Insert
  ( Insert(..)
  , OnConflict(..)
  , insert
  )
where

-- base
import Control.Exception ( throwIO )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
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
import qualified Opaleye.Manipulation as Opaleye

-- rel8
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Schema.Write ( Writes )
import Rel8.Statement.Returning ( Returning( Projection, NumberOfRowsAffected ) )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Table.Opaleye ( castTable, table, unpackspec )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
type OnConflict :: Type
data OnConflict
  = Abort     -- ^ @ON CONFLICT ABORT@
  | DoNothing -- ^ @ON CONFLICT DO NOTHING@


-- | The constituent parts of a SQL @INSERT@ statement.
type Insert :: Type -> Type
data Insert a where
  Insert :: (Selects names exprs, Writes exprs writes) =>
    { into :: TableSchema names
      -- ^ Which table to insert into.
    , rows :: [writes]
      -- ^ The rows to insert.
    , onConflict :: OnConflict
      -- ^ What to do if the inserted rows conflict with data already in the
      -- table.
    , returning :: Returning names a
      -- ^ What information to return on completion.
    }
    -> Insert a


-- | Run an @INSERT@ statement
insert :: Connection -> Insert a -> IO a
insert c Insert {into, rows, onConflict, returning} =
  case (rows, returning) of
    ([], NumberOfRowsAffected) -> pure 0
    ([], Projection _) -> pure []

    (x:xs, NumberOfRowsAffected) -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = Hasql.rowsAffected
        prepare = False
        sql = Opaleye.arrangeInsertManySql into' rows' onConflict'
          where
            into' = table $ toColumns <$> into
            rows' = toColumns <$> x :| xs

    (x:xs, Projection project) -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decoder project
        prepare = False
        sql =
          Opaleye.arrangeInsertManyReturningSql
            unpackspec
            into'
            rows'
            project'
            onConflict'
          where
            into' = table $ toColumns <$> into
            rows' = toColumns <$> x :| xs
            project' = castTable . toColumns . project . fromColumns

  where
    onConflict' =
      case onConflict of
        DoNothing -> Just Opaleye.DoNothing
        Abort     -> Nothing

    decoder :: forall exprs projection a. Serializable projection a
      => (exprs -> projection) -> Hasql.Result [a]
    decoder _ = Hasql.rowList (parse @projection @a)
