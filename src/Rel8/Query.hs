{-# language BlockArguments #-}
{-# language DisambiguateRecordFields #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Query where

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.String ( fromString )
import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( Connection )
import qualified Database.PostgreSQL.Simple.FromRow as Database.PostgreSQL.Simple
import qualified Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.MonadQuery
import qualified Rel8.Optimize
import Rel8.SimpleConstraints
import Rel8.Table
import Rel8.TableSchema
import Rel8.Unconstrained
import {-# source #-} Rel8.FromRow


-- | The type of @SELECT@able queries. You generally will not explicitly use
-- this type, instead preferring to be polymorphic over any 'MonadQuery m'.
-- Functions like 'select' will instantiate @m@ to be 'Query' when they run
-- queries.
newtype Query a = Query ( Opaleye.Query a )
  deriving ( Functor, Applicative )


instance Monad Query where
  return = pure
  Query ( Opaleye.QueryArr f ) >>= g = Query $ Opaleye.QueryArr \input ->
    case ( f input ) of
      ( a, primQuery, tag ) ->
        case g a of
          Query ( Opaleye.QueryArr h ) ->
            h ( (), primQuery, tag )


instance MonadQuery Query where
  liftOpaleye =
    Query

  toOpaleye ( Query q ) =
    q


-- | Run a @SELECT@ query, returning all rows.
select
  :: ( FromRow row haskell, MonadIO m, Recontextualise row Id )
  => Connection -> Query row -> m [ haskell ]
select = select_forAll


select_forAll
  :: forall row haskell m
   . ( FromRow row haskell, MonadIO m, Recontextualise row Id )
  => Connection -> Query row -> m [ haskell ]
select_forAll conn query =
  maybe
    ( return [] )
    ( liftIO . Database.PostgreSQL.Simple.queryWith_ ( queryParser query ) conn . fromString )
    ( selectQuery query )


queryParser
  :: ( FromRow sql haskell, Recontextualise sql Id )
  => Query sql
  -> Database.PostgreSQL.Simple.RowParser haskell
queryParser ( Query q ) =
  Opaleye.prepareRowParser
    queryRunner
    ( case Opaleye.runSimpleQueryArrStart q () of
        ( b, _, _ ) ->
          b
    )


queryRunner
  :: forall row haskell
   . ( FromRow row haskell, Recontextualise row Id )
  => Opaleye.FromFields row haskell
queryRunner =
  Opaleye.QueryRunner ( void unpackspec ) rowParser ( const True )


unpackspec
  :: ( Table row, Context row ~ Expr Query, Recontextualise row Id )
  => Opaleye.Unpackspec row row
unpackspec =
  Opaleye.Unpackspec $ Opaleye.PackMap \f ->
    traverseTable @Id ( traverseC ( traversePrimExpr f ) )


-- | Run an @INSERT@ statement
insert :: MonadIO m => Connection -> Insert result -> m result
insert connection Insert{ into, values, onConflict, returning } =
  liftIO
    ( Opaleye.runInsert_
        connection
        ( toOpaleyeInsert into values returning )
    )

  where

    toOpaleyeInsert
      :: forall schema result value
       . ( Context value ~ Expr Query
         , Context schema ~ ColumnSchema
         , Recontextualise schema Id
         , MapTable ( From Query ) schema ~ value
         , Recontextualise schema ( From Query )
         , Recontextualise value Id
         )
      => TableSchema schema
      -> [ value ]
      -> Returning schema result
      -> Opaleye.Insert result
    toOpaleyeInsert into_ iRows returning_ =
      Opaleye.Insert
        { iTable = ddlTable into_ ( writer into_ )
        , iRows
        , iReturning = opaleyeReturning returning_
        , iOnConflict
        }

      where

        iOnConflict :: Maybe Opaleye.OnConflict
        iOnConflict =
          case onConflict of
            DoNothing ->
              Just Opaleye.DoNothing

            Abort ->
              Nothing


writer
  :: forall value schema
   . ( Context value ~ Expr Query
     , Context schema ~ ColumnSchema
     , Table schema
     , Selects Query schema value
     , MapTable ( From Query ) schema ~ value
     )
  => TableSchema schema -> Opaleye.Writer value schema
writer into_ =
  let
    go
      :: forall f list
       . ( Functor list, Applicative f )
      => ( ( list Opaleye.PrimExpr, String ) -> f () )
      -> list value
      -> f ()
    go f xs =
      void
        ( traverseTableWithIndexC
            @Unconstrained
            @Id
            @schema
            @schema
            ( \i ->
                traverseC \c@ColumnSchema{ columnName } ->
                  c <$ f ( toPrimExpr . toColumn . flip field ( reverseFieldMapping @_ @( From Query ) i ) <$> xs, columnName )
            )
            ( tableColumns into_ )
        )

  in
  Opaleye.Writer ( Opaleye.PackMap go )


opaleyeReturning :: Returning schema result -> Opaleye.Returning schema result
opaleyeReturning returning =
  case returning of
    NumberOfRowsInserted ->
      Opaleye.Count

    Projection f ->
      Opaleye.ReturningExplicit
        queryRunner
        ( f . mapTable @( From Query ) ( mapC ( column . columnName ) ) )


ddlTable :: TableSchema schema -> Opaleye.Writer value schema -> Opaleye.Table value schema
ddlTable schema writer_ =
  toOpaleyeTable schema writer_ ( Opaleye.View ( tableColumns schema ) )


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: * -> * where
  Insert
    :: Selects Query schema value
    => { into :: TableSchema schema
         -- ^ Which table to insert into.
       , values :: [ value ]
         -- ^ The rows to insert.
       , onConflict :: OnConflict
         -- ^ What to do if the inserted rows conflict with data already in the
         -- table.
       , returning :: Returning schema result
         -- ^ What information to return on completion.
       }
    -> Insert result


-- | @Returning@ describes what information to return when an @INSERT@
-- statement completes.
data Returning schema a where
  -- | Just return the number of rows inserted.
  NumberOfRowsInserted :: Returning schema Int64

  -- | Return a projection of the rows inserted. This can be useful if your
  -- insert statement increments sequences by using default values.
  --
  -- >>> :t insert Insert{ returning = Projection fooId }
  -- IO [ FooId ]
  Projection
    :: ( Selects Query schema row
       , Context row ~ Context projection
       , FromRow projection a
       , Recontextualise projection Id
       )
    => ( row -> projection )
    -> Returning schema [ a ]


data OnConflict
  = Abort
  | DoNothing


selectQuery
  :: forall a
   . ( Context a ~ Expr Query, Recontextualise a Id )
  => Query a -> Maybe String
selectQuery ( Query opaleye ) =
  showSqlForPostgresExplicit

  where

    showSqlForPostgresExplicit =
      case Opaleye.runQueryArrUnpack unpackspec opaleye of
        ( x, y, z ) ->
          Opaleye.formatAndShowSQL
            ( x
            , Rel8.Optimize.optimize ( Opaleye.optimize y )
            , z
            )


delete :: MonadIO m => Connection -> Delete from returning -> m returning
delete c Delete{ from, deleteWhere, returning } =
  liftIO ( Opaleye.runDelete_ c ( go from deleteWhere returning ) )

  where

    go
      :: forall schema r row
       . ( Context schema ~ ColumnSchema
         , Context row ~ Expr Query
         , MapTable ( From Query ) schema ~ row
         , Recontextualise schema ( From Query )
         )
      => TableSchema schema
      -> ( row -> Expr Query Bool )
      -> Returning schema r
      -> Opaleye.Delete r
    go schema deleteWhere_ returning_ =
      Opaleye.Delete
        { dTable = ddlTable schema ( Opaleye.Writer ( pure () ) )
        , dWhere =
            Opaleye.Column
              . toPrimExpr
              . deleteWhere_
              . mapTable @( From Query ) ( mapC ( column . columnName ) )
        , dReturning = opaleyeReturning returning_
        }


data Delete from return where
  Delete
    :: Selects Query from row
    => { from :: TableSchema from
       , deleteWhere :: row -> Expr Query Bool
       , returning :: Returning from return
       }
    -> Delete from return


update :: MonadIO m => Connection -> Update target returning -> m returning
update connection Update{ target, set, updateWhere, returning } =
  liftIO ( Opaleye.runUpdate_ connection ( go target set updateWhere returning ) )

  where

    go
      :: forall returning target row
       . ( Context target ~ ColumnSchema
         , Context row ~ Expr Query
         , MapTable ( From Query ) target ~ row
         , Recontextualise target ( From Query )
         , Recontextualise target Id
         , Recontextualise row Id
         )
      => TableSchema target
      -> ( row -> row )
      -> ( row -> Expr Query Bool )
      -> Returning target returning
      -> Opaleye.Update returning
    go target_ set_ updateWhere_ returning_ =
      Opaleye.Update
        { uTable =
            ddlTable target_ ( writer target_ )

        , uReturning =
            opaleyeReturning returning_

        , uWhere =
            Opaleye.Column
              . toPrimExpr
              . updateWhere_
              . mapTable @( From Query ) ( mapC ( column . columnName ) )

        , uUpdateWith =
            set_ . mapTable @( From Query ) ( mapC ( column . columnName ) )
        }


data Update target returning where
  Update
    :: Selects Query target row
    => { target :: TableSchema target
       , set :: row -> row
       , updateWhere :: row -> Expr Query Bool
       , returning :: Returning target returning
       }
    -> Update target returning
