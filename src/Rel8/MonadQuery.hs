{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.MonadQuery where

import Control.Applicative ( liftA2 )
import Numeric.Natural
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.MaybeTable
import Rel8.Nest
import Rel8.SimpleConstraints
import Rel8.Table
import Rel8.TableSchema

import qualified Opaleye.Binary as Opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.Join as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye hiding ( limit )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Operators as Opaleye hiding ( restrict )
import qualified Opaleye.Order as Opaleye
import qualified Opaleye.Table as Opaleye


-- | Monads that can form SQL queries. You should write all of your queries
-- against this type class for maximum composability.
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
each :: ( MonadQuery m, Selects m schema row ) => TableSchema schema -> m row
each = each_forAll


each_forAll
  :: forall m schema row
   . ( MonadQuery m
     , Selects m schema row
     )
  => TableSchema schema -> m row
each_forAll schema =
  liftOpaleye
    ( Opaleye.selectTableExplicit
        unpackspec
        ( toOpaleyeTable schema writer view )
    )

  where

    unpackspec :: Opaleye.Unpackspec row row
    unpackspec =
      Opaleye.Unpackspec $ Opaleye.PackMap \f ->
        traverseTable
          ( \( C expr ) -> C . fromPrimExpr <$> f ( toPrimExpr expr ) )


    writer :: Opaleye.Writer () row
    writer =
      Opaleye.Writer ( Opaleye.PackMap \_ _ -> pure () )


    view :: Opaleye.View row
    view =
      Opaleye.View
        ( mapTable
            ( \( C ColumnSchema{ columnName } ) ->
                C ( column columnName )
            )
            ( tableColumns schema )
        )



-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, a null 'MaybeTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
leftJoin
  :: ( MonadQuery m
     , Promote m outer outer'
     , Compatible outer ( Expr m ) nullOuter ( Null ( Expr m ) )
     , Table nullOuter
     , Compatible nullOuter ( Null ( Expr m ) ) outer ( Expr m )
     )
  => Nest m outer'
  -> ( outer -> Expr m Bool )
  -> m ( MaybeTable ( Expr m ) nullOuter )
leftJoin = leftJoin_forAll

leftJoin_forAll
  :: forall outer nullOuter outer' m
   . ( MonadQuery m
     , Promote m outer outer'
     , Compatible outer ( Expr m ) nullOuter ( Null ( Expr m ) )
     , Table nullOuter
     , Compatible nullOuter ( Null ( Expr m ) ) outer ( Expr m )
     )
  => Nest m outer'
  -> ( outer -> Expr m Bool )
  -> m ( MaybeTable ( Expr m ) nullOuter )
leftJoin_forAll joinTable condition =
  liftOpaleye $ Opaleye.QueryArr \( (), left, t ) ->
    let
      Opaleye.QueryArr rightQueryF =
        liftA2
          (,)
          ( pure ( lit False ) )
          ( toOpaleye joinTable )

      ( right, pqR, t' ) =
        rightQueryF ( (), Opaleye.Unit, t )

      ( ( tag, renamed ), ljPEsB ) =
        Opaleye.run
          ( Opaleye.runUnpackspec
              unpackColumns
              ( Opaleye.extractLeftJoinFields 2 t' )
              right
          )

    in ( MaybeTable
           { isNull = tag
           , maybeTable = mapTable ( \( C expr ) -> C ( retype expr ) ) renamed
           }
       , Opaleye.Join
           Opaleye.LeftJoin
           ( toPrimExpr ( condition renamed ) )
           []
           ljPEsB
           left
           pqR
       , Opaleye.next t'
       )

  where

    unpackColumns :: Opaleye.Unpackspec ( Expr m Bool, outer' ) ( Expr m Bool, outer )
    unpackColumns =
      Opaleye.Unpackspec $ Opaleye.PackMap \f ( tag, outer' ) -> do
        tag' <-
          f ( toPrimExpr tag )

        outer <-
          traverseTable
            ( \( C a ) -> C . fromPrimExpr <$> f ( toPrimExpr a ) )
            outer'

        return ( fromPrimExpr tag', outer )


-- | Combine the results of two queries of the same type.
--
-- @union a b@ is the same as the SQL statement @x UNION b@.
union
  :: ( MonadQuery m, Promote m a a' )
  => Nest m a' -> Nest m a' -> m a
union = union_forAll


union_forAll
  :: forall a' a m
   . ( MonadQuery m
     , Promote m a a'
     )
  => Nest m a' -> Nest m a' -> m a
union_forAll l r =
  liftOpaleye
    ( Opaleye.unionExplicit
        binaryspec
        ( toOpaleye ( mapTable ( C . demote . toColumn) <$> l ) )
        ( toOpaleye ( mapTable ( C . demote . toColumn ) <$> r ) )
    )

  where

    binaryspec :: Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f ( a, b ) ->
        zipTablesWithM
          ( \( C x ) ( C y ) -> C . fromPrimExpr <$> f ( toPrimExpr x, toPrimExpr y ) )
          a
          b


-- | Select all distinct rows from a query, removing duplicates.
--
-- @distinct q@ is equivalent to the SQL statement @SELECT DISTINCT q@
distinct :: ( MonadQuery m, a `IsTableIn` m ) => m a -> m a
distinct = distinct_forAll


distinct_forAll
  :: forall a m
   . ( MonadQuery m, a `IsTableIn` m )
  => m a -> m a
distinct_forAll query =
  liftOpaleye ( Opaleye.distinctExplicit distinctspec ( toOpaleye query ) )

  where

    distinctspec :: Opaleye.Distinctspec a a
    distinctspec =
      Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f a ->
        traverseTable
          ( \( C x ) -> C . fromPrimExpr <$> f ( Nothing, toPrimExpr x ) )
          a


-- | @limit n@ select at most @n@ rows from a query.
--
-- @limit n@ is equivalent to the SQL @LIMIT n@.
limit :: MonadQuery m => Natural -> m a -> m a
limit n query =
  liftOpaleye
    ( Opaleye.limit
        ( fromIntegral n )
        ( toOpaleye query )
    )


-- | @offset n@ drops the first @n@ rows from a query.
--
-- @offset n@ is equivalent to the SQL @OFFSET n@.
offset :: MonadQuery m => Natural -> m a -> m a
offset n query =
  liftOpaleye
    ( Opaleye.offset
        ( fromIntegral n )
        ( toOpaleye query )
    )


-- | Drop any rows that don't match a predicate.
--
-- @where_ expr@ is equivalent to the SQL @WHERE expr@.
where_ :: MonadQuery m => Expr m Bool -> m ()
where_ x =
  liftOpaleye $ Opaleye.QueryArr \( (), left, t ) ->
    ( (), Opaleye.restrict ( toPrimExpr x ) left, t )
