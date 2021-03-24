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
  , runAggregation
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Aggregates, Col(..) )
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Aggregate ( listAggExpr, nonEmptyAggExpr, sgroupByExpr )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( htabulate, hfield, hdicts, hspecs )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Table, Columns, toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Type.Eq ( DBEq )


-- | Group equal tables together. This works by aggregating each column in the
-- given table with 'groupByExpr'.
groupBy :: forall exprs. EqTable exprs => exprs -> Aggregate exprs
groupBy (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield dicts field of
    Dict -> case hfield hspecs field of
      SSpec {nullability} -> case hfield exprs field of
        DB expr -> Aggregation $ sgroupByExpr nullability expr
  where
    dicts = hdicts @(Columns exprs) @(ConstrainDBType DBEq)


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
listAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ listAggExpr a)
    (pure exprs)


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Table Expr exprs => exprs -> Aggregate (NonEmptyTable exprs)
nonEmptyAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ nonEmptyAggExpr a)
    (pure exprs)


runAggregation :: Aggregates aggregates exprs => aggregates -> Aggregate exprs
runAggregation = fromColumns . toColumns
