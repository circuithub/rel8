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

import qualified Opaleye.Internal.Unpackspec as O
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromJust)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
-- import Database.PostgreSQL.Simple.Streaming
--        (queryWith_, streamWithOptionsAndParser_, defaultFoldOptions)
import qualified Opaleye as O
import qualified Opaleye.Internal.RunQuery as O
import qualified Opaleye.Internal.Print as Pr
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Sql as O (sql)
import qualified Opaleye.Internal.Optimize as O
import qualified Opaleye.Internal.PrimQuery as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Tag as O
import Rel8.Internal.Expr
import Rel8.Internal.Operators
import Rel8.Internal.Table
import Rel8.Internal.SqlTransformations
import Rel8.Internal.Types (Insert, QueryResult)
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
-- it is possible to map over the results of a query and perform subsequent
-- queries on the same connection. If you need this, use 'streamCursor'.
stream :: (MonadResource m, MonadMask m) => Connection -> QueryRunner m
stream conn parser query = queryWith_ parser conn query

-- | Stream the results of a query and fetch the results using a PostgreSQL
-- cursor. This variation is slightly more expensive, but has the benefit that
-- you can run other queries on the same connection while results are being
-- streamed.
streamCursor :: (MonadResource m, MonadMask m) => Connection -> QueryRunner m
streamCursor conn parser query =
  streamWithOptionsAndParser_ defaultFoldOptions parser conn query

--------------------------------------------------------------------------------
-- | Given a database query, execute this query and return a 'Stream' of
-- results.
select
  :: (MonadIO m, Table rows results)
  => QueryRunner m -> O.Query rows -> Stream (Of results) m ()
select io query =
  runQueryExplicit io queryRunner query

insert
  :: (BaseTable table, MonadIO m)
  => Connection -> [table Insert] -> m Int64
insert conn rows =
  liftIO (O.runInsertMany conn tableDefinition rows)

insertReturning
  :: (BaseTable table, MonadIO m)
  => QueryRunner m
  -> [table Insert]
  -> Stream (Of (table QueryResult)) m ()
insertReturning io rows =
  runInsertManyReturningExplicit io queryRunner tableDefinition rows id

insert1Returning
  :: (BaseTable table, MonadIO m)
  => Connection -> table Insert -> m (table QueryResult)
insert1Returning c =
  liftIO .
  runResourceT . fmap fromJust . S.head_ . insertReturning (stream c) . pure

update
  :: (BaseTable table, DBBool bool, MonadIO m)
  => Connection
  -> (table Expr -> Expr bool)
  -> (table Expr -> table Expr)
  -> m Int64
update conn f up =
  liftIO $
  O.runUpdate
    conn
    tableDefinitionUpdate
    up
    (exprToColumn . toNullable . f)

updateReturning
  :: (BaseTable table, DBBool bool, MonadIO m)
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
                (\_columns -> True) -- TODO Will we support 0-column queries?

--------------------------------------------------------------------------------

runQueryExplicit
  :: Monad m
  => QueryRunner m
  -> O.QueryRunner columns haskells
  -> O.Query columns
  -> Stream (Of haskells) m ()
runQueryExplicit io qr q = maybe (return ()) (io parser) sql
  where (sql, parser) = prepareQuery qr q

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

prepareQuery
  :: O.QueryRunner columns haskells
  -> O.Query columns
  -> (Maybe Pg.Query, Pg.RowParser haskells)
prepareQuery qr@(O.QueryRunner u _ _) q = (fmap fromString sql, parser)
  where (b, sql) = showSqlExplicit u q
        parser = O.prepareRowParser qr b

showSqlExplicit
  :: O.Unpackspec s t -> O.QueryArr () s -> (s, Maybe String)
showSqlExplicit up q = case O.runSimpleQueryArrStart q () of
  (x, y, z) -> (x, formatAndShowSQL (O.collectPEs up x, O.optimize y, z))

formatAndShowSQL
  :: ([O.PrimExpr],O.PrimQuery' a,O.Tag) -> Maybe String
formatAndShowSQL =
  fmap (show . Pr.ppSql . simplifySelect . O.sql) . traverse2Of3 O.removeEmpty
  where
        -- Just a lens
        traverse2Of3
          :: Functor f
          => (a -> f b) -> (x,a,y) -> f (x,b,y)
        traverse2Of3 f (x,y,z) =
          fmap (\y' -> (x,y',z))
               (f y)
