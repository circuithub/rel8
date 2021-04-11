{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Aggregate
  ( groupBy
  , listAgg
  , nonEmptyAgg
  )
where

-- base
import Prelude

-- profunctors
import Data.Profunctor ( lmap )

-- rel8
import Rel8.Aggregate ( Aggregator, Col(..) )
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Aggregate ( groupByExpr, listAggExpr, nonEmptyAggExpr )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( htabulate, hfield, hspecs )
import Rel8.Schema.HTable.MapTable ( HMapTableField(..) )
import Rel8.Schema.HTable.Vectorize ( HVectorize(..) )
import Rel8.Schema.Spec
import Rel8.Table ( Table, toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )


-- | Group equal tables together. This works by aggregating each column in the
-- given table with 'groupByExpr'.
groupBy :: forall exprs i. EqTable exprs => (i -> exprs) -> Aggregator i exprs
groupBy f = lmap (toColumns . f) $ fromColumns $ htabulate $ \field ->
  case hfield (eqTable @exprs) field of
    Dict -> 
      case hfield hspecs field of
        SSpec{} -> 
          Aggregation $ groupByExpr \exprs ->
            case hfield exprs field of
              DB expr -> expr

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
listAgg :: Table Expr exprs => (i -> exprs) -> Aggregator i (ListTable exprs)
listAgg f = lmap (toColumns . f) $ fromColumns $ HVectorize $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    SSpec {} -> Aggregation $ listAggExpr \exprs ->
      case hfield exprs field of
        DB expr -> expr


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Table Expr exprs => (i -> exprs) -> Aggregator i (NonEmptyTable exprs)
nonEmptyAgg f = lmap (toColumns . f) $ fromColumns $ HVectorize $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    SSpec {} -> Aggregation $ nonEmptyAggExpr \exprs ->
      case hfield exprs field of
        DB expr -> expr
