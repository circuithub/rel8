{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language DisambiguateRecordFields #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Query where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.Proxy
import Data.String ( fromString )
import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( Connection )
import qualified Database.PostgreSQL.Simple.FromRow as Database.PostgreSQL.Simple
import Numeric.Natural
import qualified Opaleye ( runInsert_, Insert(..), OnConflict(..), formatAndShowSQL, runDelete_, Delete(..), runUpdate_, Update(..), valuesExplicit )
import qualified Opaleye.Binary as Opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye hiding ( limit )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Lateral as Opaleye
import qualified Opaleye.Operators as Opaleye hiding ( restrict )
import qualified Opaleye.Order as Opaleye
import qualified Opaleye.Table as Opaleye
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.MaybeTable
import qualified Rel8.Optimize
import Rel8.SimpleConstraints
import Rel8.Table
import Rel8.TableSchema
import Rel8.Unconstrained
import Rel8.FromRow


-- | The type of @SELECT@able queries. You generally will not explicitly use
-- this type, instead preferring to be polymorphic over any 'MonadQuery m'.
-- Functions like 'select' will instantiate @m@ to be 'Query' when they run
-- queries.
newtype Query a = Query ( Opaleye.Query a )
  deriving ( Functor, Applicative )


liftOpaleye :: Opaleye.Query a -> Query a
liftOpaleye =
  Query


toOpaleye :: Query a -> Opaleye.Query a
toOpaleye ( Query q ) =
  q


instance Monad Query where
  return = pure
  Query ( Opaleye.QueryArr f ) >>= g = Query $ Opaleye.QueryArr \input ->
    case ( f input ) of
      ( a, primQuery, tag ) ->
        case g a of
          Query ( Opaleye.QueryArr h ) ->
            h ( (), primQuery, tag )


-- | Run a @SELECT@ query, returning all rows.
select
  :: ( FromRow row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select = select_forAll


select_forAll
  :: forall row haskell m
   . ( FromRow row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select_forAll conn query =
  maybe
    ( return [] )
    ( liftIO . Database.PostgreSQL.Simple.queryWith_ ( queryParser query ) conn . fromString )
    ( selectQuery query )


queryParser
  :: FromRow sql haskell
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
   . FromRow row haskell
  => Opaleye.FromFields row haskell
queryRunner =
  Opaleye.QueryRunner ( void unpackspec ) rowParser ( const True )


unpackspec
  :: (ExprTable row)
  => Opaleye.Unpackspec row row
unpackspec =
  Opaleye.Unpackspec $ Opaleye.PackMap \f ->
    traverseTable (traverseC (traversePrimExpr f))


-- | Run an @INSERT@ statement
insert :: MonadIO m => Connection -> Insert result -> m result
insert connection Insert{ into, rows, onConflict, returning } =
  liftIO
    ( Opaleye.runInsert_
        connection
        ( toOpaleyeInsert into rows returning )
    )

  where

    toOpaleyeInsert
      :: forall schema result value
       . ( ExprTable value
         , Context schema ~ ColumnSchema
         , Structure value ~ Structure schema
         , Table schema
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
   . ( Context value ~ Expr
     , Context schema ~ ColumnSchema
     , Selects schema value
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
            @value
            ( \i ->
                traverseC \ColumnSchema{ columnName } -> do
                  f ( toPrimExpr . toColumn . flip hfield i . toStructure <$> xs
                    , columnName
                    )

                  return ( column columnName )
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
        ( f . mapTable ( mapC ( column . columnName ) ) )


ddlTable :: TableSchema schema -> Opaleye.Writer value schema -> Opaleye.Table value schema
ddlTable schema writer_ =
  toOpaleyeTable schema writer_ ( Opaleye.View ( tableColumns schema ) )


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: * -> * where
  Insert
    :: Selects schema value
    => { into :: TableSchema schema
         -- ^ Which table to insert into.
       , rows :: [ value ]
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
    :: ( Selects schema row
       , Table projection
       , Context row ~ Context projection
       , FromRow projection a
       )
    => ( row -> projection )
    -> Returning schema [ a ]


data OnConflict
  = Abort
  | DoNothing


selectQuery :: forall a . ExprTable a => Query a -> Maybe String
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
         , ExprTable row
         , Structure schema ~ Structure row
         , Table schema
         )
      => TableSchema schema
      -> ( row -> Expr Bool )
      -> Returning schema r
      -> Opaleye.Delete r
    go schema deleteWhere_ returning_ =
      Opaleye.Delete
        { dTable = ddlTable schema ( Opaleye.Writer ( pure () ) )
        , dWhere =
            Opaleye.Column
              . toPrimExpr
              . deleteWhere_
              . mapTable (mapC (column . columnName))
        , dReturning = opaleyeReturning returning_
        }


data Delete from return where
  Delete
    :: Selects from row
    => { from :: TableSchema from
       , deleteWhere :: row -> Expr Bool
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
         , ExprTable row
         , Structure target ~ Structure row
         , Table target
         )
      => TableSchema target
      -> ( row -> row )
      -> ( row -> Expr Bool )
      -> Returning target returning
      -> Opaleye.Update returning
    go target_ set_ updateWhere_ returning_ =
      Opaleye.Update
        { uTable =
            ddlTable target_ (writer target_)

        , uReturning =
            opaleyeReturning returning_

        , uWhere =
            Opaleye.Column
              . toPrimExpr
              . updateWhere_
              . mapTable (mapC (column . columnName))

        , uUpdateWith =
            set_ . mapTable (mapC (column . columnName))
        }


data Update target returning where
  Update
    :: Selects target row
    => { target :: TableSchema target
       , set :: row -> row
       , updateWhere :: row -> Expr Bool
       , returning :: Returning target returning
       }
    -> Update target returning


-- | Exists checks if a query returns at least one row.
--
-- @exists q@ is the same as the SQL expression @EXISTS ( q )@
exists :: Query a -> Query ( Expr Bool )
exists query =
  liftOpaleye ( lit True <$ Opaleye.restrictExists ( toOpaleye query ) )


-- | Select each row from a table definition.
--
-- This is equivalent to @FROM table@.
each :: Selects schema row => TableSchema schema -> Query row
each = each_forAll


each_forAll
  :: forall schema row
   . Selects schema row
  => TableSchema schema -> Query row
each_forAll schema =
  liftOpaleye
    ( Opaleye.selectTableExplicit
        unpackspec
        ( toOpaleyeTable schema noWriter view )
    )

  where

    noWriter :: Opaleye.Writer () row
    noWriter =
      Opaleye.Writer ( Opaleye.PackMap \_ _ -> pure () )


    view :: Opaleye.View row
    view =
      Opaleye.View
        ( mapTable
            ( mapC ( column . columnName ) )
            ( tableColumns schema )
        )


-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, a null 'MaybeTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
optional :: Query a -> Query (MaybeTable a)
optional =
  liftOpaleye . Opaleye.laterally (Opaleye.QueryArr . go) . toOpaleye

  where

    go query (i, left, tag) =
      ( MaybeTable t' a, join, Opaleye.next tag' )

      where

        ( MaybeTable t a, right, tag' ) =
          Opaleye.runSimpleQueryArr (pure <$> query) (i, tag)

        ( t', bindings ) =
          Opaleye.run $
          Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "maybe" tag') t

        join =
          Opaleye.Join Opaleye.LeftJoin (toPrimExpr $ lit True) [] bindings left right


-- | Combine the results of two queries of the same type.
--
-- @union a b@ is the same as the SQL statement @x UNION b@.
union :: ExprTable a => Query a -> Query a -> Query a
union = union_forAll


union_forAll
  :: forall a
   . ExprTable a
  => Query a -> Query a -> Query a
union_forAll l r =
  liftOpaleye
    ( Opaleye.unionExplicit
        binaryspec
        ( toOpaleye l )
        ( toOpaleye r )
    )

  where

    binaryspec :: Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f ( a, b ) ->
        zipTablesWithM
          ( zipCWithM \x y -> fromPrimExpr <$> f ( toPrimExpr x, toPrimExpr y ) )
          a
          b


-- | Select all distinct rows from a query, removing duplicates.
--
-- @distinct q@ is equivalent to the SQL statement @SELECT DISTINCT q@
distinct :: ExprTable a => Query a -> Query a
distinct = distinct_forAll


distinct_forAll :: forall a. ExprTable a => Query a -> Query a
distinct_forAll query =
  liftOpaleye ( Opaleye.distinctExplicit distinctspec ( toOpaleye query ) )

  where

    distinctspec :: Opaleye.Distinctspec a a
    distinctspec =
      Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f ->
        traverseTable (traverseC \x -> fromPrimExpr <$> f (Nothing, toPrimExpr x))


-- | @limit n@ select at most @n@ rows from a query.
--
-- @limit n@ is equivalent to the SQL @LIMIT n@.
limit :: Natural -> Query a -> Query a
limit n query =
  liftOpaleye
    ( Opaleye.limit
        ( fromIntegral n )
        ( toOpaleye query )
    )


-- | @offset n@ drops the first @n@ rows from a query.
--
-- @offset n@ is equivalent to the SQL @OFFSET n@.
offset :: Natural -> Query a -> Query a
offset n query =
  liftOpaleye
    ( Opaleye.offset
        ( fromIntegral n )
        ( toOpaleye query )
    )


-- | Drop any rows that don't match a predicate.
--
-- @where_ expr@ is equivalent to the SQL @WHERE expr@.
where_ :: Expr Bool -> Query ()
where_ x =
  liftOpaleye $ Opaleye.QueryArr \( (), left, t ) ->
    ( (), Opaleye.restrict ( toPrimExpr x ) left, t )


catMaybeTable :: MaybeTable a -> Query a
catMaybeTable MaybeTable{ nullTag, table } = do
  where_ $ not_ $ isNull nullTag
  return table


catMaybe :: Expr (Maybe a) -> Query (Expr a)
catMaybe e =
  catMaybeTable $ MaybeTable (ifThenElse_ (isNull e) (lit Nothing) (lit (Just False))) (unsafeCoerceExpr e)


values :: (ExprTable expr, Foldable f) => f expr -> Query expr
values = liftOpaleye . Opaleye.valuesExplicit unpackspec valuesspec . toList
  where
    valuesspec =
      Opaleye.Valuesspec $ Opaleye.PackMap \f () ->
        fmap fromStructure $
        htabulate (Proxy @Unconstrained) \_ -> MkC . fromPrimExpr <$> f ()
