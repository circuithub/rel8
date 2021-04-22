{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Aggregate
  ( groupBy
  , listAgg, nonEmptyAgg
  , listAggWithOrder, nonEmptyAggWithOrder
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Col(..) )
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Aggregate
  ( groupByExpr
  , listAggExpr
  , listAggExprWithOrder
  , nonEmptyAggExpr
  , nonEmptyAggExprWithOrder
  )
import Rel8.Order ( Order )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( htabulate, hfield )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Table ( Table, toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )


-- | Group equal tables together. This works by aggregating each column in the
-- given table with 'groupByExpr'.
groupBy :: forall exprs. EqTable exprs => exprs -> Aggregate exprs
groupBy (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield (eqTable @exprs) field of
    Dict -> case hfield exprs field of
      DB expr -> Aggregation $ groupByExpr expr


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
listAgg :: Table Expr a => a -> Aggregate (ListTable a)
listAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ listAggExpr a)
    (pure exprs)


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Table Expr a => a -> Aggregate (NonEmptyTable a)
nonEmptyAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ nonEmptyAggExpr a)
    (pure exprs)


listAggWithOrder :: Table Expr b
  => (a -> o) -> (a -> b) -> Order o -> a -> Aggregate (ListTable b)
listAggWithOrder o b order a = fromColumns $
  hvectorize
    (\_ (Identity (DB x)) -> Aggregation $ listAggExprWithOrder (o a) order x)
    (pure (toColumns (b a)))


nonEmptyAggWithOrder :: Table Expr b
  => (a -> o) -> (a -> b) -> Order o -> a -> Aggregate (NonEmptyTable b)
nonEmptyAggWithOrder o b order a = fromColumns $
  hvectorize
    (\_ (Identity (DB x)) -> Aggregation $ nonEmptyAggExprWithOrder (o a) order x)
    (pure (toColumns (b a)))
