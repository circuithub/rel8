{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Table.Aggregate
  ( groupBy, groupByOn
  , listAgg, listAggOn, nonEmptyAgg, nonEmptyAggOn
  , listCat, listCatOn, nonEmptyCat, nonEmptyCatOn
  , filterWhere, filterWhereOptional
  , orderAggregateBy
  , optionalAggregate
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye

-- profunctors
import Data.Profunctor (dimap, lmap)

-- rel8
import Rel8.Aggregate
  ( Aggregator,  Aggregator' (Aggregator), Aggregator1
  , toAggregator
  )
import Rel8.Aggregate.Fold (Fallback (Fallback))
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate
  ( filterWhereExplicit
  , groupByExprOn
  , slistAggExpr
  , slistCatExpr
  , snonEmptyAggExpr
  , snonEmptyCatExpr
  )
import Rel8.Expr.Opaleye (toColumn, toPrimExpr)
import Rel8.Order (Order (Order))
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable (HTable, hfield, hspecs, htabulateA)
import Rel8.Schema.HTable.Vectorize (htraverseVectorP, hvectorizeA)
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Spec ( Spec( Spec, info ) )
import Rel8.Table (Table, toColumns, fromColumns)
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe (MaybeTable, makeMaybeTable, justTable, nothingTable)
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.Opaleye (ifPP)
import Rel8.Type.Eq ( DBEq )


-- | Group equal tables together. This works by aggregating each column in the
-- given table with 'groupByExpr'.
--
-- For example, if we have a table of items, we could group the items by the
-- order they belong to:
--
-- @
-- itemsByOrder :: Query (OrderId Expr, ListTable Expr (Item Expr))
-- itemsByOrder =
--   aggregate
--     do
--       orderId <- groupByOn (.orderId)
--       items <- listAgg
--       pure (orderId, items)
--     do
--       each itemSchema
-- @
groupBy :: forall a. EqTable a => Aggregator1 a a
groupBy = dimap toColumns fromColumns (hgroupBy (eqTable @a))


-- | Applies 'groupBy' to the columns selected by the given function.
groupByOn :: EqTable a => (i -> a) -> Aggregator1 i a
groupByOn f = lmap f groupBy


hgroupBy :: HTable t => t (Dict (Sql DBEq)) -> Aggregator1 (t Expr) (t Expr)
hgroupBy eqs = htabulateA $ \field -> case hfield eqs field of
  Dict -> groupByExprOn (`hfield` field)


-- | 'filterWhere' allows an 'Aggregator' to filter out rows from the input
-- query before considering them for aggregation. Note that because the
-- predicate supplied to 'filterWhere' could return 'Rel8.false' for every
-- row, 'filterWhere' needs an 'Aggregator' as opposed to an 'Aggregator1', so
-- that it can return a default value in such a case. For a variant of
-- 'filterWhere' that can work with 'Aggregator1's, see 'filterWhereOptional'.
filterWhere :: Table Expr a
  => (i -> Expr Bool) -> Aggregator i a -> Aggregator' fold i a
filterWhere = filterWhereExplicit ifPP


-- | A variant of 'filterWhere' that can be used with an 'Aggregator1'
-- (upgrading it to an 'Aggregator' in the process). It returns
-- 'nothingTable' in the case where the predicate matches zero rows.
filterWhereOptional :: Table Expr a
  => (i -> Expr Bool) -> Aggregator' fold i a -> Aggregator' fold' i (MaybeTable Expr a)
filterWhereOptional f (Aggregator _ aggregator) =
  Aggregator (Fallback nothingTable) $
    Opaleye.filterWhereInternal makeMaybeTable (toColumn . toPrimExpr . f) aggregator


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
-- ordersWithItems :: Query (Order Expr, ListTable Expr (Item Expr))
-- ordersWithItems = do
--   order <- each orderSchema
--   items <- aggregate listAgg (itemsFromOrder order)
--   return (order, items)
-- @
listAgg :: Table Expr a => Aggregator' fold a (ListTable Expr a)
listAgg =
  fromColumns <$>
  hvectorizeA \Spec {info} field ->
    lmap ((`hfield` field) . toColumns) $ slistAggExpr info


-- | Applies 'listAgg' to the columns selected by the given function.
listAggOn :: Table Expr a => (i -> a) -> Aggregator' fold i (ListTable Expr a)
listAggOn f = lmap f listAgg


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Table Expr a => Aggregator1 a (NonEmptyTable Expr a)
nonEmptyAgg =
  fromColumns <$>
  hvectorizeA \Spec {info} field ->
    lmap ((`hfield` field) . toColumns) $ snonEmptyAggExpr info


-- | Applies 'nonEmptyAgg' to the columns selected by the given function.
nonEmptyAggOn :: Table Expr a
  => (i -> a) -> Aggregator1 i (NonEmptyTable Expr a)
nonEmptyAggOn f = lmap f nonEmptyAgg


-- | Concatenate lists into a single list.
listCat :: Table Expr a
  => Aggregator' fold (ListTable Expr a) (ListTable Expr a)
listCat = dimap toColumns fromColumns $
  htraverseVectorP (\field -> case hfield hspecs field of
    Spec {info} -> slistCatExpr info)


-- | Applies 'listCat' to the list selected by the given function.
listCatOn :: Table Expr a
  => (i -> ListTable Expr a) -> Aggregator' fold i (ListTable Expr a)
listCatOn f = lmap f listCat


-- | Concatenate non-empty lists into a single non-empty list.
nonEmptyCat :: Table Expr a
  => Aggregator1 (NonEmptyTable Expr a) (NonEmptyTable Expr a)
nonEmptyCat = dimap toColumns fromColumns $
  htraverseVectorP (\field -> case hfield hspecs field of
    Spec {info} -> snonEmptyCatExpr info)


-- | Applies 'nonEmptyCat' to the non-empty list selected by the given
-- function.
nonEmptyCatOn :: Table Expr a
  => (i -> NonEmptyTable Expr a) -> Aggregator1 i (NonEmptyTable Expr a)
nonEmptyCatOn f = lmap f nonEmptyCat


-- | Order the values within each aggregation in an `Aggregator` using the
-- given ordering. This is only relevant for aggregations that depend on the
-- order they get their elements, like `Rel8.listAgg` and `Rel8.stringAgg`.
orderAggregateBy :: Order i -> Aggregator' fold i a -> Aggregator' fold i a
orderAggregateBy (Order order) (Aggregator fallback aggregator) =
  Aggregator fallback $ Opaleye.orderAggregate order aggregator


-- | 'optionalAggregate' upgrades an 'Aggregator1' into an 'Aggregator' by
-- having it return 'nothingTable' when aggregating over an empty collection
-- of rows.
optionalAggregate :: Table Expr a
  => Aggregator' fold i a -> Aggregator' fold' i (MaybeTable Expr a)
optionalAggregate = toAggregator nothingTable . fmap justTable
