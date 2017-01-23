module Rel8.IO
  ( select
  , update
  , updateReturning
  , insertReturning
  , insert
  , insert1Returning
  , delete
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Database.PostgreSQL.Simple (Connection)
import qualified Opaleye as O
import qualified Opaleye.Internal.RunQuery as O
import Rel8.Internal.Expr
import Rel8.Internal.Operators
import Rel8.Internal.Table
import Rel8.Internal.Types (Insert, QueryResult)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

--------------------------------------------------------------------------------
-- | Given a database query, execute this query and return a 'Stream' of
-- results.
select
  :: (MonadIO m, Table rows results)
  => Connection -> O.Query rows -> Stream (Of results) m ()
select connection query = do
  results <-
    liftIO $
    O.runQueryExplicit
      queryRunner
      connection
      query
  S.each results

insert
  :: (BaseTable table, MonadIO m)
  => Connection -> [table Insert] -> m Int64
insert conn rows =
  liftIO (O.runInsertMany conn tableDefinition rows)

insertReturning
  :: (BaseTable table, MonadIO m)
  => Connection -> [table Insert] -> Stream (Of (table QueryResult)) m ()
insertReturning conn rows =
  do results <-
       liftIO (O.runInsertManyReturningExplicit queryRunner conn tableDefinition rows id)
     S.each results

insert1Returning
  :: (BaseTable table, MonadIO m)
  => Connection -> table Insert -> m (table QueryResult)
insert1Returning c = fmap fromJust . S.head_ . insertReturning c . pure

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
  => Connection
  -> (table Expr -> Expr bool)
  -> (table Expr -> table Expr)
  -> Stream (Of (table QueryResult)) m ()
updateReturning conn f up = do
  r <-
    liftIO $
    O.runUpdateReturningExplicit
      queryRunner
      conn
      tableDefinitionUpdate
      up
      (exprToColumn . toNullable . f)
      id
  S.each r

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
