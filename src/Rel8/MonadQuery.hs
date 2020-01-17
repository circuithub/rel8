{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.MonadQuery where

import Control.Applicative ( liftA2 )
import Data.Proxy
import Numeric.Natural
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.MaybeTable
import Rel8.Nest
import Rel8.Rewrite
import Rel8.TableSchema
import Rel8.Top
import Rel8.ZipLeaves

import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Join as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Operators as Opaleye
import qualified Opaleye.Table as Opaleye


-- | The class of monads that can form SQL queries, along with the corresponding
-- expression type.
class Monad m => MonadQuery m where
  liftOpaleye :: Opaleye.Query a -> m a

  toOpaleye :: m a -> Opaleye.Query a


instance MonadQuery m => MonadQuery ( Nest m ) where
  liftOpaleye =
    Nest . liftOpaleye

  toOpaleye ( Nest m ) =
    toOpaleye m


-- | Exists checks if a query returns at least one row.
--
-- @exists q@ is the same as the SQL expression @EXISTS ( q )@
exists :: MonadQuery m => m a -> m ( Expr m Bool )
exists query =
  liftOpaleye ( lit True <$ Opaleye.restrictExists ( toOpaleye query ) )


-- | Select each row from a table definition.
--
-- This is equivalent to @FROM table@.
each
  :: forall m schema row
   . ( MonadQuery m
     , Rewrite ColumnSchema ( Expr m ) schema row
     , ZipLeaves row row ( Expr m ) ( Expr m )
     , CanZipLeaves row row Top
     )
  => TableSchema schema -> m row
each schema =
  liftOpaleye ( Opaleye.selectTableExplicit unpackspec table )

  where

    unpackspec :: Opaleye.Unpackspec row row
    unpackspec =
      Opaleye.Unpackspec $ Opaleye.PackMap \f row ->
        zipLeaves
          ( Proxy @Top )
          ( \( C expr ) _ -> fmap ( C . Expr ) ( f ( toPrimExpr expr ) ) )
          row
          row


    table :: Opaleye.Table () row
    table =
      case tableSchema schema of
        Nothing ->
          Opaleye.Table ( tableName schema ) tableFields

        Just s ->
          Opaleye.TableWithSchema s ( tableName schema ) tableFields


    tableFields :: Opaleye.TableFields () row
    tableFields =
      Opaleye.TableProperties writer view


    writer :: Opaleye.Writer () row
    writer =
      Opaleye.Writer ( Opaleye.PackMap \_ _ -> pure () )


    view :: Opaleye.View row
    view =
      Opaleye.View
        ( rewrite
            ( \( C ColumnSchema{ columnName } ) ->
                C ( Expr ( Opaleye.BaseTableAttrExpr columnName ) )
            )
            ( tableColumns schema )
        )



-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, 'nullTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
leftJoin
  :: forall outer outer' m
   . ( MonadQuery m, CanZipLeaves outer' outer Top, ZipLeaves outer' outer ( Expr ( Nest m ) ) ( Expr m ) )
  => Nest m outer'
  -> ( outer -> Expr m Bool )
  -> m ( MaybeTable outer )
leftJoin joinTable condition =
  liftOpaleye $ Opaleye.QueryArr \( (), left, t ) ->
    let
      Opaleye.QueryArr rightQueryF =
        liftA2
          (,)
          ( pure ( lit False ) )
          ( toOpaleye joinTable )

      ( right, pqR, t' ) =
        rightQueryF ( (), Opaleye.Unit, t )

      ( ( {- TODO -} _tag, renamed ), ljPEsB ) =
        Opaleye.run
          ( Opaleye.runUnpackspec
              unpackColumns
              ( Opaleye.extractLeftJoinFields 2 t' )
              right
          )

    in ( MaybeTable () renamed
       , Opaleye.Join
           Opaleye.LeftJoin
           ( case condition renamed of
              Expr a -> a
           )
           []
           ljPEsB
           left
           pqR
       , Opaleye.next t'
       )

  where

    unpackColumns :: Opaleye.Unpackspec ( Expr m Bool, outer' ) ( Expr n Bool, outer )
    unpackColumns =
      Opaleye.Unpackspec $ Opaleye.PackMap \f ( tag, outer' ) -> do
        tag' <-
          f ( toPrimExpr tag )

        outer <-
          zipLeaves
            ( Proxy @Top )
            ( \( C a) _ -> C . Expr <$> f ( toPrimExpr a ) )
            outer'
            outer'

        return ( Expr tag', outer )


-- | Combine the results of two queries of the same type.
--
-- @union a b@ is the same as the SQL statement @x UNION b@.
union :: m a -> m a -> m a
union = undefined


-- | Select all distinct rows from a query, removing duplicates.
--
-- @distinct q@ is equivalent to the SQL statement @SELECT DISTINCT q@
distinct :: m a -> m a
distinct = undefined


-- | @limit n@ select at most @n@ rows from a query.
--
-- @limit n@ is equivalent to the SQL @LIMIT n@.
limit :: Natural -> m a -> m a
limit = undefined


-- | @offset n@ drops the first @n@ rows from a query.
--
-- @offset n@ is equivalent to the SQL @OFFSET n@.
offset :: Natural -> m a -> m a
offset = undefined


-- | Drop any rows that don't match a predicate.
--
-- @where_ expr@ is equivalent to the SQL @WHERE expr@.
where_ :: MonadQuery m => Expr m Bool -> m ()
where_ =
  undefined
