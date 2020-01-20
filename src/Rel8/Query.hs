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
import Database.PostgreSQL.Simple ( Connection )
import qualified Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.MonadQuery
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
  :: ( Compatible row ( Expr Query ) row ( Expr Query ), FromRow row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select = select_forAll


select_forAll
  :: forall row haskell m
   . ( Compatible row ( Expr Query ) row ( Expr Query ), FromRow row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select_forAll c ( Query query ) =
  liftIO ( Opaleye.runSelectExplicit ( queryRunner ) c query )

  where


queryRunner
  :: forall row haskell
   . ( Compatible row ( Expr Query ) row ( Expr Query ), FromRow row haskell )
  => Opaleye.FromFields row haskell
queryRunner =
  Opaleye.QueryRunner ( void unpackspec ) rowParser ( const True )

  where

    unpackspec :: Opaleye.Unpackspec row row
    unpackspec =
      Opaleye.Unpackspec $ Opaleye.PackMap \f ->
        traverseTable
          ( \( C x ) -> C . Expr <$> f ( toPrimExpr x ) )


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
         , CompatibleTables schema value
         , Context schema ~ ColumnSchema
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
   . ( CompatibleTables schema value
     , Context value ~ Expr Query
     , Context schema ~ ColumnSchema
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
            @schema
            @schema
            ( \i c@( C ( ColumnSchema{ columnName } ) ) ->
                c <$ f ( toPrimExpr . toColumn . flip field ( transferField i ) <$> xs, columnName )
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
        ( f . mapTable \( C ColumnSchema{ columnName } ) -> C ( Expr ( Opaleye.BaseTableAttrExpr columnName ) )
        )


ddlTable :: TableSchema schema -> Opaleye.Writer value schema -> Opaleye.Table value schema
ddlTable schema writer_ =
  toOpaleyeTable schema writer_ ( Opaleye.View ( tableColumns schema ) )


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: * -> * where
  Insert
    :: ( Table schema
       , Table value
       , Context schema ~ ColumnSchema
       , Context value ~ Expr Query
       , Compatible schema ColumnSchema schema ColumnSchema
       , Compatible schema ColumnSchema value ( Expr Query )
       )
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
    :: ( CompatibleTables row schema
       , Context row ~ Expr Query
       , Context schema ~ ColumnSchema
       , FromRow projection a
       )
    => ( row -> projection )
    -> Returning schema [ a ]


data OnConflict
  = Abort
  | DoNothing


showSQL
  :: forall a
   . ( Context a ~ Expr Query, Table a )
  => Query a -> Maybe String
showSQL ( Query opaleye ) =
  Opaleye.showSqlExplicit unpackspec opaleye

  where

    unpackspec :: Opaleye.Unpackspec a a
    unpackspec =
      Opaleye.Unpackspec $ Opaleye.PackMap \f row ->
        traverseTable
          ( \( C expr ) -> C . Expr <$> f ( toPrimExpr expr ) )
          row


delete :: MonadIO m => Connection -> Delete from returning -> m returning
delete c Delete{ from, deleteWhere, returning } =
  liftIO ( Opaleye.runDelete_ c ( go from deleteWhere returning ) )

  where

    go
      :: forall schema r row
       . ( CompatibleTables row schema, Context schema ~ ColumnSchema, Context row ~ Expr Query )
      => TableSchema schema
      -> ( row -> Expr Query Bool )
      -> Returning schema r
      -> Opaleye.Delete r
    go schema deleteWhere_ returning_ =
      Opaleye.Delete
        { dTable = ddlTable schema ( Opaleye.Writer ( pure () ) )
        , dWhere = Opaleye.Column . toPrimExpr . deleteWhere_ . mapTable ( \( C ColumnSchema{ columnName } ) -> C ( Expr ( Opaleye.BaseTableAttrExpr columnName ) ) )
        , dReturning = opaleyeReturning returning_
        }


data Delete from return where
  Delete
    :: ( CompatibleTables row from, Context from ~ ColumnSchema, Context row ~ Expr Query )
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
       . ( CompatibleTables row target
         , CompatibleTables target row
         , Context target ~ ColumnSchema
         , Context row ~ Expr Query
         )
      => TableSchema target
      -> ( row -> row )
      -> ( row -> Expr Query Bool )
      -> Returning target returning
      -> Opaleye.Update returning
    go target_ set_ updateWhere_ returning_ =
      Opaleye.Update
        { uTable = ddlTable target_ ( writer target_ )
        , uReturning = opaleyeReturning returning_
        , uWhere = Opaleye.Column . toPrimExpr . updateWhere_ . mapTable ( \( C ColumnSchema{ columnName } ) -> C ( Expr ( Opaleye.BaseTableAttrExpr columnName ) ) )
        , uUpdateWith = set_ . mapTable ( \( C ColumnSchema{ columnName } ) -> C ( Expr ( Opaleye.BaseTableAttrExpr columnName ) ) )
        }


data Update target returning where
  Update
    :: ( CompatibleTables row target
       , CompatibleTables target row
       , Context target ~ ColumnSchema
       , Context row ~ Expr Query
       )
    => { target :: TableSchema target
       , set :: row -> row
       , updateWhere :: row -> Expr Query Bool
       , returning :: Returning target returning
       }
    -> Update target returning
