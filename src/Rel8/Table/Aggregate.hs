{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Aggregate
  ( groupBy, hgroupBy
  , listAgg, nonEmptyAgg
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Aggregates, Col( A ) )
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.Expr.Aggregate
  ( groupByExpr
  , slistAggExpr
  , snonEmptyAggExpr
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HTable, hfield, htabulate )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Spec ( SSpec( SSpec, info ) )
import Rel8.Schema.Spec.Constrain ( ConstrainSpec )
import Rel8.Table ( toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Type.Eq ( DBEq )


-- | Group equal tables together. This works by aggregating each column in the
-- given table with 'groupByExpr'.
groupBy :: forall exprs aggregates. (EqTable exprs, Aggregates aggregates exprs)
  => exprs -> aggregates
groupBy = fromColumns . hgroupBy (eqTable @exprs) . toColumns


hgroupBy :: HTable t
  => t (Dict (ConstrainSpec (Sql DBEq))) -> t (Col Expr) -> t (Col Aggregate)
hgroupBy eqs exprs = fromColumns $ htabulate $ \field ->
  case hfield eqs field of
    Dict -> case hfield exprs field of
      E expr -> A $ groupByExpr expr


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
listAgg :: Aggregates aggregates exprs => exprs -> ListTable aggregates
listAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\SSpec {info} (Identity (E a)) -> A $ slistAggExpr info a)
    (pure exprs)


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Aggregates aggregates exprs => exprs -> NonEmptyTable aggregates
nonEmptyAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\SSpec {info} (Identity (E a)) -> A $ snonEmptyAggExpr info a)
    (pure exprs)
