{-# LANGUAGE RankNTypes #-}
module Rel8.IO
  (
    -- * @SELECT@
    select

    -- * @INSERT@
  , insert
  , insertReturning
  , insert1Returning

    -- * @UPDATE@
  , update
  , updateReturning

    -- * @DELETE@
  , delete

    -- * Streaming results
  , QueryRunner
  , stream
  , streamCursor
  ) where

import Control.Lens (nullOf)
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromJust)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple (defaultFoldOptions)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import Database.PostgreSQL.Simple.Streaming
       (queryWith_, streamWithOptionsAndParser_)
import qualified Opaleye as O
import qualified Opaleye.Internal.RunQuery as O
import qualified Opaleye.Internal.Table as O
import Rel8.Internal.Expr
import Rel8.Internal.Operators
import Rel8.Internal.Table hiding (columns)
import Rel8.Internal.Types
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

--------------------------------------------------------------------------------
-- | A suitable way to execute a query and stream the results.
--
-- @rel8@ provides 'stream' and 'streamCursor', but you are free to provide
-- your own query runners.
type QueryRunner m = forall a. Pg.RowParser a -> Pg.Query -> Stream (Of a) m ()

-- | Stream the results of a single query incrementally. This runner essentially
-- blocks subsequent queries on a connection until the stream is exhausted. Thus
-- it is /not/ possible to map over the results of a query and perform subsequent
-- queries on the same connection. If you need this, use 'streamCursor'.
--
-- @
-- 'stream' = 'queryWith_'
-- @
stream :: (MonadResource m) => Connection -> QueryRunner m
stream conn parser query = queryWith_ parser conn query

-- | Stream the results of a query and fetch the results using a PostgreSQL
-- cursor. This variation is slightly more expensive, but has the benefit that
-- you can run other queries on the same connection while results are being
-- streamed.
-- @
-- 'streamCursor' = 'streamWithOptionsAndParser_' 'defaultFoldOptions'
-- @
streamCursor :: (MonadResource m, MonadMask m) => Connection -> QueryRunner m
streamCursor conn parser query =
  streamWithOptionsAndParser_ defaultFoldOptions parser conn query

--------------------------------------------------------------------------------
-- | Given a database query, execute this query and return a 'Stream' of
-- results. This runs a @SELECT@ statement.
select
  :: (MonadIO m, Table rows results)
  => QueryRunner m -> O.Query rows -> Stream (Of results) m ()
select io query =
  runQueryExplicit io queryRunner query

-- | Insert rows into a 'BaseTable'. This runs a @INSERT@ statement.
insert
  :: (BaseTable table, MonadIO m)
  => Connection -> [table Insert] -> m Int64
insert conn rows =
  liftIO (O.runInsertMany conn tableDefinition rows)

-- | Insert rows into a 'BaseTable', and return the inserted rows. This runs a
-- @INSERT ... RETURNING@ statement, and be useful to immediately retrieve
-- any default values (such as automatically generated primary keys).
insertReturning
  :: (BaseTable table, MonadIO m)
  => QueryRunner m
  -> [table Insert]
  -> Stream (Of (table QueryResult)) m ()
insertReturning io rows =
  runInsertManyReturningExplicit io queryRunner tableDefinition rows id

-- | Insert a single row 'BaseTable', and return the inserted row. This runs a
-- @INSERT ... RETURNING@ statement, and be useful to immediately retrieve
-- any default values (such as automatically generated primary keys).
--
-- *WARNING* - it is assumed that the @INSERT@ will insert a row assuming
-- no exception happens. This assumption can be violated with triggers on
-- tables that prevent the row from being inserted. If this could happen,
-- consider 'insertReturning'.
insert1Returning
  :: (BaseTable table, MonadIO m)
  => Connection -> table Insert -> m (table QueryResult)
insert1Returning c =
  liftIO .
  runResourceT . fmap fromJust . S.head_ . insertReturning (stream c) . pure

-- | Update rows in a 'BaseTable'. Corresponds to @UPDATE@.
update
  :: (BaseTable table, DBBool bool, MonadIO m)
  => Connection
  -> (table Expr -> Expr bool)
     -- ^ A predicate specifying which rows should be updated.
  -> (table Expr -> table Expr)
     -- ^ The transformation to apply to each row. This function is given the
     -- rows current value as input.
  -> m Int64
     -- ^ The amount of rows updated.
update conn f up =
  liftIO $
  O.runUpdate
    conn
    tableDefinitionUpdate
    up
    (exprToColumn . toNullable . f)

-- | Update rows in a 'BaseTable' and return the results. Corresponds to
-- @UPDATE ... RETURNING@.
updateReturning
  :: (BaseTable table, DBBool bool)
  => QueryRunner m
  -> (table Expr -> Expr bool)
  -> (table Expr -> table Expr)
  -> Stream (Of (table QueryResult)) m ()
updateReturning io f up = do
  runUpdateReturningExplicit
    io
    queryRunner
    tableDefinitionUpdate
    up
    (exprToColumn . toNullable . f)
    id

-- | Given a 'BaseTable' and a predicate, @DELETE@ all rows that match.
delete
  :: (BaseTable table, DBBool bool)
  => Connection -> (table Expr -> Expr bool) -> IO Int64
delete conn f =
  O.runDelete conn tableDefinition (exprToColumn . toNullable . f)

queryRunner :: Table a b => O.QueryRunner a b
queryRunner =
  O.QueryRunner (void unpackColumns)
                (const rowParser)
                (Prelude.not . nullOf (expressions . traverse))

--------------------------------------------------------------------------------

runQueryExplicit
  :: Monad m
  => QueryRunner m
  -> O.QueryRunner columns haskells
  -> O.Query columns
  -> Stream (Of haskells) m ()
runQueryExplicit io qr q = maybe (return ()) (io parser) sql
  where (sql, parser) = O.prepareQuery qr q

runInsertManyReturningExplicit
  :: Monad m
  => QueryRunner m
  -> O.QueryRunner columnsReturned haskells
  -> O.Table columnsW columnsR
  -> [columnsW]
  -> (columnsR -> columnsReturned)
  -> Stream (Of haskells) m ()
runInsertManyReturningExplicit io qr t columns r =
  case NEL.nonEmpty columns of
    Nothing -> return ()
    Just columns' ->
      io parser (fromString (O.arrangeInsertManyReturningSql u t columns' r))
  where
    O.QueryRunner u _ _ = qr
    parser = O.prepareRowParser qr (r v)
    O.Table _ (O.TableProperties _ (O.View v)) = t

runUpdateReturningExplicit
  :: QueryRunner m
  -> O.QueryRunner columnsReturned haskells
  -> O.Table columnsW columnsR
  -> (columnsR -> columnsW)
  -> (columnsR -> O.Column O.PGBool)
  -> (columnsR -> columnsReturned)
  -> Stream (Of haskells) m ()
runUpdateReturningExplicit io qr t up cond r =
  io parser (fromString (O.arrangeUpdateReturningSql u t up cond r))
  where
    O.QueryRunner u _ _ = qr
    parser = O.prepareRowParser qr (r v)
    O.Table _ (O.TableProperties _ (O.View v)) = t
