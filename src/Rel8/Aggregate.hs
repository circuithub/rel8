{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}

module Rel8.Aggregate
  ( Aggregate(..)
  , aggregate
  , groupBy
  , boolAnd
  , boolOr
  , count
  , countStar
  , countDistinct
  , listAgg
  , nonEmptyAgg
  , some
  , many
  , aggregateAllExprs
  ) where

-- base
import Data.Int ( Int64 )
import Data.List.NonEmpty ( NonEmpty )

-- rel8
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import Rel8.Context ( Context( Column ), Meta( Meta ) )
import Rel8.Expr ( Column( ExprColumn ), Expr( Expr ) )
import Rel8.HTable ( hmap, htraverse )
import Rel8.HTable.HMapTable ( HMapTable( HMapTable ), Precompose( Precompose ) )
import Rel8.Query ( Query, mapOpaleye )
import Rel8.Table ( Table( toColumns ), fromColumns )
import Rel8.Table.ListTable ( ListTable( ListTable ) )
import Rel8.Table.MaybeTable ( maybeTable, optional )
import Rel8.Table.NonEmptyTable ( NonEmptyTable( NonEmptyTable ) )


-- | An @Aggregate a@ describes how to aggregate @Table@s of type @a@. You can
-- unpack an @Aggregate@ back to @a@ by running it with 'aggregate'. As
-- @Aggregate@ is an 'Applicative' functor, you can combine @Aggregate@s using
-- the normal @Applicative@ combinators, or by working in @do@ notation with
-- @ApplicativeDo@.
newtype Aggregate a = Aggregate a


instance Functor Aggregate where
  fmap f (Aggregate a) = Aggregate $ f a


instance Applicative Aggregate where
  pure = Aggregate
  Aggregate f <*> Aggregate a = Aggregate $ f a


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: forall a. Table Expr a => Query (Aggregate a) -> Query a
aggregate = mapOpaleye $ Opaleye.aggregate aggregator
  where
    aggregator :: Opaleye.Aggregator (Aggregate a) a
    aggregator = Opaleye.Aggregator $ Opaleye.PackMap \f (Aggregate x) ->
      fromColumns <$> htraverse (g f) (toColumns x)

    g :: forall m x. Applicative m => ((Maybe (Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct), Opaleye.PrimExpr) -> m Opaleye.PrimExpr) -> Column Expr x -> m (Column Expr x)
    g f (ExprColumn (Expr x)) = ExprColumn . Expr <$> traverseAggrExpr f x


-- | Aggregate a value by grouping by it. @groupBy@ is just a synonym for
-- 'pure', but sometimes being explicit can help the readability of your code.
groupBy :: a -> Aggregate a
groupBy = pure


aggregateAllExprs :: Opaleye.AggrOp -> Expr a -> Aggregate (Expr b)
aggregateAllExprs op (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll op a []


-- | Corresponds to @bool_and@.
boolAnd :: Expr Bool -> Aggregate (Expr Bool)
boolAnd (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrBoolAnd a []


-- | Corresponds to @bool_or@.
boolOr :: Expr Bool -> Aggregate (Expr Bool)
boolOr (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrBoolOr a []


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
count :: Expr a -> Aggregate (Expr Int64)
count (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrCount a []


-- | Corresponds to @COUNT(*)@.
countStar :: Aggregate (Expr Int64)
countStar = count (0 :: Expr Int64)


-- | Count the number of distinct occurances of a single column. Corresponds to
-- @COUNT(DISTINCT a)@
countDistinct :: Expr a -> Aggregate (Expr Int64)
countDistinct (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrDistinct Opaleye.AggrCount a []


traverseAggrExpr :: Applicative f
  => ((Maybe (Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct), Opaleye.PrimExpr) -> f Opaleye.PrimExpr)
  -> Opaleye.PrimExpr
  -> f Opaleye.PrimExpr
traverseAggrExpr f = \case
  Opaleye.AggrExpr a b c d ->
    f (Just (b, d, a), c)

  Opaleye.AttrExpr symbol ->
    -- TODO Test me
    f (Nothing, Opaleye.AttrExpr symbol)

  Opaleye.BaseTableAttrExpr attribute ->
    -- TODO Test me
    f (Nothing, Opaleye.BaseTableAttrExpr attribute)

  Opaleye.CompositeExpr primExpr x ->
    Opaleye.CompositeExpr <$> traverseAggrExpr f primExpr <*> pure x

  Opaleye.BinExpr x primExpr1 primExpr2 ->
    Opaleye.BinExpr x <$> traverseAggrExpr f primExpr1 <*> traverseAggrExpr f primExpr2

  Opaleye.UnExpr x primExpr ->
    Opaleye.UnExpr x <$> traverseAggrExpr f primExpr

  Opaleye.CaseExpr cases def ->
    Opaleye.CaseExpr <$> traverse (traverseBoth (traverseAggrExpr f)) cases <*> traverseAggrExpr f def
    where traverseBoth g (x, y) = (,) <$> g x <*> g y

  Opaleye.ListExpr elems ->
    Opaleye.ListExpr <$> traverse (traverseAggrExpr f) elems

  Opaleye.ParamExpr p primExpr ->
    Opaleye.ParamExpr p <$> traverseAggrExpr f primExpr

  Opaleye.FunExpr name params ->
    Opaleye.FunExpr name <$> traverse (traverseAggrExpr f) params

  Opaleye.CastExpr t primExpr ->
    Opaleye.CastExpr t <$> traverseAggrExpr f primExpr

  Opaleye.ArrayExpr elems ->
    Opaleye.ArrayExpr <$> traverse (traverseAggrExpr f) elems

  Opaleye.RangeExpr a b c ->
    Opaleye.RangeExpr a <$> traverseBoundExpr (traverseAggrExpr f) b <*> traverseBoundExpr (traverseAggrExpr f) c
    where
      traverseBoundExpr g = \case
        Opaleye.Inclusive primExpr -> Opaleye.Inclusive <$> g primExpr
        Opaleye.Exclusive primExpr -> Opaleye.Exclusive <$> g primExpr
        other                      -> pure other

  Opaleye.ArrayIndex x i ->
    Opaleye.ArrayIndex <$> traverseAggrExpr f x <*> traverseAggrExpr f i

  other ->
    -- All other constructors that don't contain any PrimExpr's.
    pure other


-- | Aggregate rows into a single row containing an array of all aggregated
-- rows. This can be used to associate multiple rows with a single row, without
-- changing the over cardinality of the query. This allows you to essentially
-- return a tree-like structure from queries.
--
-- For example, if we have a table of orders and each orders contains multiple
-- items, we could aggregate the table of orders, pairing each order with its
-- items:
--
-- @
-- ordersWithItems :: Query (Order Expr, ListTable (Item Expr))
-- ordersWithItems = do
--   order <- each orderSchema
--   items <- aggregate $ listAgg <$> itemsFromOrder order
--   return (order, items)
-- @
listAgg :: Table Expr exprs => exprs -> Aggregate (ListTable exprs)
listAgg = Aggregate . ListTable . HMapTable . hmap (Precompose . go) . toColumns
  where
    go :: Column Expr ('Meta a) -> Column Expr ('Meta [a])
    go (ExprColumn (Expr a)) =
      ExprColumn $ Expr $
        Opaleye.FunExpr "row" [Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrArr a []]


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Table Expr exprs => exprs -> Aggregate (NonEmptyTable exprs)
nonEmptyAgg = Aggregate . NonEmptyTable . HMapTable . hmap (Precompose . go) . toColumns
  where
    go :: Column Expr ('Meta a) -> Column Expr ('Meta (NonEmpty a))
    go (ExprColumn (Expr a)) =
      ExprColumn $ Expr $ Opaleye.FunExpr "row" [Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrArr a []]


-- | Aggregate a 'Query' into a 'NonEmptyTable'. If the supplied query returns
-- 0 rows, this function will produce a 'Query' that is empty - that is, will
-- produce zero @NonEmptyTable@s. If the supplied @Query@ does return rows,
-- @some@ will return exactly one row, with a @NonEmptyTable@ collecting all
-- returned rows.
--
-- @some@ is analogous to 'Control.Applicative.some' from
-- @Control.Applicative@.
some :: Table Expr exprs => Query exprs -> Query (NonEmptyTable exprs)
some = aggregate . fmap nonEmptyAgg


-- | Aggregate a 'Query' into a 'ListTable'. If the supplied query returns 0
-- rows, this function will produce a 'Query' that returns one row containing
-- the empty @ListTable@. If the supplied @Query@ does return rows, @many@ will
-- return exactly one row, with a @ListTable@ collecting all returned rows.
-- 
-- @many@ is analogous to 'Control.Applicative.many' from
-- @Control.Applicative@.
many :: Table Expr exprs => Query exprs -> Query (ListTable exprs)
many = fmap (maybeTable mempty id) . optional . aggregate . fmap listAgg
