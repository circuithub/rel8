{-# language BlockArguments #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}

module Rel8.IO
  ( select
  , prepare
  , insert
  , Insert( Insert, into, values, onConflict, returning )
  , OnConflict( DoNothing, Abort )
  , delete
  , Delete( Delete, from, deleteWhere, returning )
  , update
  , Update( Update, target, set, updateWhere, returning )
  , Returning( NumberOfRowsAffected, Projection )
  ) where

-- base
import Control.Applicative ( Const( Const ), getConst )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Int ( Int64 )
import Data.Kind ( Type )
import Data.Monoid ( Any( Any ), getAny )
import Data.String ( fromString )

-- opaleye
import Opaleye.Internal.HaskellDB.PrimQuery ( Literal( OtherLit ) )
import qualified Opaleye.Internal.Manipulation as Opaleye ( Returning( Count, ReturningExplicit ) )
import qualified Opaleye.Internal.Optimize as Opaleye ( optimize )
import Opaleye.Internal.Print ( ppValuesRow )
import Opaleye.Internal.QueryArr ( runQueryArrUnpack )
import Opaleye.Internal.RunQuery ( QueryRunner( QueryRunner ) )
import Opaleye.Internal.Sql ( sqlExpr )
import qualified Opaleye.Manipulation as Opaleye
  ( Delete( Delete, dTable, dWhere, dReturning )
  , Insert( Insert, iTable, iRows, iReturning, iOnConflict )
  , OnConflict( DoNothing )
  , Update( Update, uTable, uReturning, uWhere, uUpdateWith )
  , runDelete_
  , runInsert_
  , runUpdate_
  )
import Opaleye.RunQuery ( runQueryExplicit )
import Opaleye.Sql ( formatAndShowSQL )

-- postgresql-simple
import Database.PostgreSQL.Simple ( Connection, execute_, queryWith_ )

-- rel8
import Data.Indexed.Functor.Identity ( unHIdentity )
import Data.Indexed.Functor.Representable ( htabulate )
import Data.Indexed.Functor.Traversable ( hsequence, htraverse )
import Rel8.Column ( Column( Column ), lit, toOpaleyeColumn )
import Rel8.Query ( Query, toOpaleye, unpackspec )
import Rel8.Row ( Row( Row ), lit, toColumns )
import Rel8.Schema ( TableSchema, table )
import Rel8.Table ( Table, rowParser )

-- transformers
import Control.Monad.Trans.State.Strict ( State, evalState, get, modify' )


-- | Run a @SELECT@ query, returning all rows.
select :: (MonadIO m, Table a) => Connection ->  Query () (Row a) -> m [a]
select c = liftIO . runQueryExplicit queryRunner c . toOpaleye


-- | @PREPARE@ a @SELECT@ statement, parameterized by a row of parameters.
prepare
  :: forall a b m
   . (MonadIO m, Table a, Table b)
  => Connection -> String -> (Row a -> Query () (Row b)) -> m (a -> m [b])
prepare c name q =
  case showSql parameterizedQuery of
    Nothing ->
      return $ const $ return []

    Just sql -> do
      _ <- liftIO $ execute_ c $ fromString $ "PREPARE " <> name <> " AS " <> sql
      return \a -> do
        let Const exprs = htraverse (\(Column prim) -> Const [sqlExpr prim]) (toColumns (Rel8.Row.lit a))
        liftIO $ queryWith_ rowParser c (fromString $ "EXECUTE " <> name <> " " <> show (ppValuesRow exprs))

  where

    parameterizedQuery = q (evalState mkRow 1)

    mkRow :: Table x => State Int (Row x)
    mkRow = fmap Row $ hsequence $ htabulate $ const $ Compose column

    column = do
      n <- get <* modify' succ
      return (Rel8.Column.lit $ OtherLit $ "$" <> show n)


queryRunner :: Table a  => QueryRunner (Row a) a
queryRunner = QueryRunner (void unpackspec) (const rowParser) hasColumns


hasColumns :: Table a => Row a -> Bool
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
  NumberOfRowsAffected -> Opaleye.Count
  Projection f         -> Opaleye.ReturningExplicit queryRunner f


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: Type -> Type where
  Insert
    :: Table table
    => { into :: TableSchema table
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
  NumberOfRowsAffected :: Returning table Int64

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
    => { from :: TableSchema from
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
    => { target :: TableSchema row
       , set :: Row row -> Row row
       , updateWhere :: Row row -> Row Bool
       , returning :: Returning row returning
       }
    -> Update returning


showSql :: Table a => Query () (Row a) -> Maybe String
showSql =
  formatAndShowSQL . optimise . runQueryArrUnpack unpackspec . toOpaleye

  where

    optimise (a, b, c) = (a, Opaleye.optimize b, c)
