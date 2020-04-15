{-# language BlockArguments #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Rel8.IO where

import Control.Applicative ( Const(..) )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Int ( Int64 )
import Data.Kind ( Type )
import Data.Monoid ( Any(..) )
import Database.PostgreSQL.Simple ( Connection )
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Manipulation as Opaleye
import qualified Opaleye.RunQuery as Opaleye
import Rel8.Column
import Rel8.Query ( Query, toOpaleye, unpackspec )
import Rel8.Row
import Rel8.Schema ( Schema, table )
import Rel8.Table ( Table, Pattern, rowParser )


-- | Run a @SELECT@ query, returning all rows.
select :: (MonadIO m, Table a) => Connection ->  Query () (Row a) -> m [a]
select c = liftIO . Opaleye.runQueryExplicit queryRunner c . toOpaleye


queryRunner :: Table a  => Opaleye.QueryRunner (Row a) a
queryRunner = Opaleye.QueryRunner (void unpackspec) (const rowParser) hasColumns


hasColumns :: HTraversable (Pattern a) => Row a -> Bool
hasColumns (Row f) = getAny $ getConst $ htraverse (\_ -> Const $ Any True) f


-- | Run an @INSERT@ statement
insert :: MonadIO m => Connection -> Insert result -> m result
insert connection Insert{ into, values, onConflict, returning } =
  liftIO $ Opaleye.runInsert_ connection $ toOpaleyeInsert into values returning

  where

    toOpaleyeInsert into_ iRows returning_ =
      Opaleye.Insert
        { iTable = table into_
        , iRows
        , iReturning = opaleyeReturning returning_
        , iOnConflict
        }

      where

        iOnConflict :: Maybe Opaleye.OnConflict
        iOnConflict =
          case onConflict of
            DoNothing -> Just Opaleye.DoNothing
            Abort     -> Nothing


opaleyeReturning :: Returning schema result -> Opaleye.Returning (Row schema) result
opaleyeReturning = \case
  NumberOfRowsInserted -> Opaleye.Count
  Projection f         -> Opaleye.ReturningExplicit queryRunner f


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: Type -> Type where
  Insert
    :: Table table
    => { into :: Schema table
         -- ^ Which table to insert into.
       , values :: [ Row table ]
         -- ^ The rows to insert.
       , onConflict :: OnConflict
         -- ^ What to do if the inserted rows conflict with data already in the
         -- table.
       , returning :: Returning table a
         -- ^ What information to return on completion.
       }
    -> Insert a


-- | @Returning@ describes what information to return when an @INSERT@
-- statement completes.
data Returning table a where
  -- | Just return the number of rows inserted.
  NumberOfRowsInserted :: Returning table Int64

  -- | Return a projection of the rows inserted. This can be useful if your
  -- insert statement increments sequences by using default values.
  --
  -- >>> :t insert Insert{ returning = Projection fooId }
  -- IO [ FooId ]
  Projection :: Table a => (Row table -> Row a) -> Returning table [a]


data OnConflict
  = Abort
  | DoNothing


delete :: MonadIO m => Connection -> Delete returning -> m returning
delete c Delete{ from, deleteWhere, returning } =
  liftIO $ Opaleye.runDelete_ c $ go deleteWhere returning

  where

    go deleteWhere_ returning_ =
      Opaleye.Delete
        { dTable = table from
        , dWhere =
            toOpaleyeColumn
              . unHIdentity
              . toColumns
              . deleteWhere_
        , dReturning = opaleyeReturning returning_
        }


data Delete return where
  Delete
    :: Table from
    => { from :: Schema from
       , deleteWhere :: Row from -> Row Bool
       , returning :: Returning from return
       }
    -> Delete return


update :: MonadIO m => Connection -> Update returning -> m returning
update connection Update{ target, set, updateWhere, returning } =
  liftIO $ Opaleye.runUpdate_ connection $ go target set updateWhere returning

  where

    go target_ set_ deleteWhere returning_ =
      Opaleye.Update
        { uTable =
            table target_

        , uReturning =
            opaleyeReturning returning_

        , uWhere =
            toOpaleyeColumn
              . unHIdentity
              . toColumns
              . deleteWhere

        , uUpdateWith =
            set_
        }


data Update returning where
  Update
    :: Table row
    => { target :: Schema row
       , set :: Row row -> Row row
       , updateWhere :: Row row -> Row Bool
       , returning :: Returning row returning
       }
    -> Update returning
