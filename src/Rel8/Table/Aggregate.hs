{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Aggregate
  ( groupBy
  , headAgg
  , listAgg, nonEmptyAgg
  , listAggWithOrder, nonEmptyAggWithOrder
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Col( A ) )
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.Expr.Aggregate
  ( groupByExpr
  , headAggExpr
  , listAggExpr
  , listAggExprWithOrder
  , nonEmptyAggExpr
  , nonEmptyAggExprWithOrder
  )
import Rel8.Order ( Order )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( hfield, hmap, htabulate )
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
      E expr -> A $ groupByExpr expr


-- | Keep only the first row.
headAgg :: Table Expr a => a -> Aggregate a
headAgg = fromColumns . hmap (\(E a) -> A $ headAggExpr a) . toColumns


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
    (\_ (Identity (E a)) -> A $ listAggExpr a)
    (pure exprs)


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Table Expr a => a -> Aggregate (NonEmptyTable a)
nonEmptyAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (E a)) -> A $ nonEmptyAggExpr a)
    (pure exprs)


listAggWithOrder :: Table Expr a
  => Order o -> o -> a -> Aggregate (ListTable a)
listAggWithOrder order o (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (E x)) -> A $ listAggExprWithOrder order o x)
    (pure exprs)


nonEmptyAggWithOrder :: Table Expr a
  => Order o -> o -> a -> Aggregate (NonEmptyTable a)
nonEmptyAggWithOrder order o (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (E x)) -> A $ nonEmptyAggExprWithOrder order o x)
    (pure exprs)
