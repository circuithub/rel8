{-# language BlockArguments #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Query where

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Database.PostgreSQL.Simple ( Connection )
import qualified Opaleye
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
  :: ( Compatible row row, FromRow row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select = select_forAll


select_forAll
  :: forall row haskell m
   . ( Compatible row row, FromRow row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select_forAll c ( Query query ) =
  liftIO ( Opaleye.runSelectExplicit fromFields c query )

  where

    fromFields :: Opaleye.FromFields row haskell
    fromFields =
      Opaleye.QueryRunner ( void unpackspec ) rowParser ( const True )


    unpackspec :: Opaleye.Unpackspec row row
    unpackspec =
      Opaleye.Unpackspec $ Opaleye.PackMap \f ->
        traverseTable
          ( \( C x ) -> C . Expr <$> f ( toPrimExpr x ) )


-- Run an @INSERT@ statement
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
       . ( Table schema
         , Compatible schema value
         , Compatible schema schema
         , Context schema ~ ColumnSchema
         , Context value ~ Expr Query
         )
      => TableSchema schema
      -> [ value ]
      -> Returning result
      -> Opaleye.Insert result
    toOpaleyeInsert into_ iRows returning_ =
      Opaleye.Insert { iTable, iRows, iReturning, iOnConflict }

      where

        iTable :: Opaleye.Table value ()
        iTable =
          toOpaleyeTable into writer ( Opaleye.View () )


        writer :: Opaleye.Writer value ()
        writer =
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


        iReturning :: Opaleye.Returning () result
        iReturning =
          case returning_ of
            NumberOfRowsInserted ->
              Opaleye.Count


        iOnConflict :: Maybe Opaleye.OnConflict
        iOnConflict =
          case onConflict of
            DoNothing ->
              Just Opaleye.DoNothing

            Abort ->
              Nothing


data Insert :: * -> * where
  Insert
    :: ( Table schema
       , Table value
       , Compatible schema value
       , Context schema ~ ColumnSchema
       , Compatible schema schema
       , Context value ~ Expr Query
       )
    => { into :: TableSchema schema
       , values :: [ value ]
       , onConflict :: OnConflict
       , returning :: Returning result
       }
    -> Insert result


data Returning a where
  NumberOfRowsInserted :: Returning Int64
  Projection :: Returning a


data OnConflict
  = Abort
  | DoNothing


showSQL
  :: forall a
   . ( Context a ~ Expr Query, Compatible a a )
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
