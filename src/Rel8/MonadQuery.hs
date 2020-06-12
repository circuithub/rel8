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
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.MonadQuery where

import Control.Applicative ( Const(..) )
import Data.Functor.Identity
import Numeric.Natural
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.SimpleConstraints
import Rel8.Table
import Rel8.TableSchema

import qualified Opaleye.Binary as Opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye hiding ( limit )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Operators as Opaleye hiding ( restrict )
import qualified Opaleye.Order as Opaleye
import qualified Opaleye.Table as Opaleye


-- | Monads that can form SQL queries. You should write all of your queries
-- against this type class for maximum composability.
class Monad m => MonadQuery m where
  liftOpaleye :: Opaleye.Query a -> m a

  toOpaleye :: m a -> Opaleye.Query a


-- | Exists checks if a query returns at least one row.
--
-- @exists q@ is the same as the SQL expression @EXISTS ( q )@
exists :: MonadQuery m => m a -> m ( Expr Bool )
exists query =
  liftOpaleye ( lit True <$ Opaleye.restrictExists ( toOpaleye query ) )


-- | Select each row from a table definition.
--
-- This is equivalent to @FROM table@.
each :: ( MonadQuery m, Selects schema row ) => TableSchema schema -> m row
each = each_forAll


each_forAll
  :: forall m schema row
   . ( MonadQuery m
     , Selects schema row
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
      Opaleye.Unpackspec
        $ Opaleye.PackMap \f ->
            fmap runIdentity . traverseTable @Id ( traverseC ( traversePrimExpr f ) ) . Identity


    writer :: Opaleye.Writer () row
    writer =
      Opaleye.Writer ( Opaleye.PackMap \_ _ -> pure () )


    view :: Opaleye.View row
    view =
      Opaleye.View
        ( mapTable
            @From
            ( mapC ( column . columnName ) )
            ( tableColumns schema )
        )



-- -- | Select all rows from another table that match a given predicate. If the
-- -- predicate is not satisfied, a null 'MaybeTable' is returned.
-- --
-- -- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
-- leftJoin
--   :: ( MonadQuery m
--      , Recontextualise outer Null
--      , Context ( MapTable Null outer ) ~ Null Expr
--      , Context outer ~ Expr
--      )
--   => m outer
--   -> ( outer -> Expr Bool )
--   -> m ( MaybeTable outer )
-- leftJoin = leftJoin_forAll

-- leftJoin_forAll
--   :: forall outer nullOuter m
--    . ( MonadQuery m
--      , Recontextualise outer Null
--      , MapTable Null outer ~ nullOuter
--      , Context nullOuter ~ Null ( Context outer )
--      )
--   => m outer
--   -> ( outer -> Expr Bool )
--   -> m ( MaybeTable outer )
-- leftJoin_forAll joinTable condition =
--   liftOpaleye $ Opaleye.QueryArr \( (), left, t ) ->
--     let
--       Opaleye.QueryArr rightQueryF =
--         liftA2
--           (,)
--           ( pure ( lit False ) )
--           ( toOpaleye joinTable )

--       ( right, pqR, t' ) =
--         rightQueryF ( (), Opaleye.Unit, t )

--       ( ( tag, renamed ), ljPEsB ) =
--         Opaleye.run
--           ( Opaleye.runUnpackspec
--               unpackColumns
--               ( Opaleye.extractLeftJoinFields 2 t' )
--               right
--           )

--     in ( MaybeTable
--            { nullTag =
--                liftNull tag
--            , maybeTable =
--                mapTable @Null ( mapC retype ) renamed
--            }
--        , Opaleye.Join
--            Opaleye.LeftJoin
--            ( toPrimExpr ( condition renamed ) )
--            []
--            ljPEsB
--            left
--            pqR
--        , Opaleye.next t'
--        )

--   where

--     unpackColumns :: Opaleye.Unpackspec ( Expr Bool, outer ) ( Expr Bool, outer )
--     unpackColumns =
--       Opaleye.Unpackspec $ Opaleye.PackMap \f ( tag, outer' ) -> do
--         tag' <-
--           f ( toPrimExpr tag )

--         outer <-
--           traverseC ( fmap demote . traversePrimExpr f )

--         return ( fromPrimExpr tag', outer )


-- | Combine the results of two queries of the same type.
--
-- @union a b@ is the same as the SQL statement @x UNION b@.
union :: (MonadQuery m, Table a, Context a ~ Expr) => m a -> m a -> m a
union = union_forAll


union_forAll
  :: forall a m
   . ( MonadQuery m
     , Context a ~ Expr
     , Table a
     )
  => m a -> m a -> m a
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
      Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f ->
        fmap runIdentity
          . traverseTable @Id
              ( traverseC \x -> fromPrimExpr <$> f ( Nothing, toPrimExpr x ) )
          . Identity


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
where_ :: MonadQuery m => Expr Bool -> m ()
where_ x =
  liftOpaleye $ Opaleye.QueryArr \( (), left, t ) ->
    ( (), Opaleye.restrict ( toPrimExpr x ) left, t )


catNulls
  :: forall a b m
   . ( MonadQuery m
     , Context a ~ Expr
     , Context b ~ Expr
     , MapTable NotNull a ~ b
     , Recontextualise a NotNull
     )
  => m a -> m b
catNulls q = do
  x <-
    q

  let
    allNotNull :: [ Expr Bool ]
    allNotNull =
      getConst
        ( runIdentity
            <$> traverseTable
                  @Id
                  ( traverseC ( \expr -> Const [ isNull ( retype expr ) ] ) )
                  ( Identity x )
        )

  where_ ( and_ allNotNull )

  return ( mapTable @NotNull ( mapC retype ) x )
