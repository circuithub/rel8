{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Aggregate.Legacy
  ( Aggregates
  , aggregate
  , aggregateTabulation
  , groupBy
  , headAgg
  , listAgg
  , nonEmptyAgg

  , AggregateADT, aggregateADT
  , AggregateHKD, aggregateHKD
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import GHC.Generics ( Rep )
import Prelude

-- opaleye
import qualified Opaleye.Aggregate as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate, Aggregates, Col( A ) )
import Rel8.Expr ( Col( E ), Expr )
import Rel8.Expr.Aggregate
  ( groupByExpr
  , headAggExpr
  , listAggExpr, nonEmptyAggExpr
  )
import Rel8.Generic.Construction ( GGAggregate', ggaggregate' )
import Rel8.Generic.Table ( GAlgebra )
import Rel8.Kind.Algebra ( Algebra( Sum ) )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( hmap, hfield, htabulate )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Schema.Kind ( Rel8able )
import Rel8.Table ( toColumns, fromColumns )
import Rel8.Table.ADT ( ConstructableADT, ADT( ADT ), ADTRep )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.HKD ( ConstructableHKD, HKD( HKD ), HKDRep )
import Rel8.Table.List ( ListTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.Opaleye ( aggregator )
import Rel8.Tabulate ( Tabulation )
import qualified Rel8.Tabulate


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: Aggregates aggregates exprs => Query aggregates -> Query exprs
aggregate = mapOpaleye (Opaleye.aggregate aggregator) . fmap (fromColumns . toColumns)


aggregateTabulation
  :: (EqTable k, Aggregates aggregates exprs)
  => (t -> aggregates) -> Tabulation k t -> Tabulation k exprs
aggregateTabulation f =
  Rel8.Tabulate.aggregateTabulation . fmap (fromColumns . toColumns . f)


-- | Group equal tables together. This works by aggregating each column in the
-- given table with 'groupByExpr'.
groupBy :: forall exprs aggregates. (EqTable exprs, Aggregates aggregates exprs)
  => exprs -> aggregates
groupBy (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield (eqTable @exprs) field of
    Dict -> case hfield exprs field of
      E expr -> A $ groupByExpr expr


-- | Keep only the first row's values.
headAgg :: Aggregates aggregates exprs => exprs -> aggregates
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
listAgg :: Aggregates aggregates exprs => exprs -> ListTable aggregates
listAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (E a)) -> A $ listAggExpr a)
    (pure exprs)


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: Aggregates aggregates exprs => exprs -> NonEmptyTable aggregates
nonEmptyAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (E a)) -> A $ nonEmptyAggExpr a)
    (pure exprs)


type AggregateADT :: Rel8able -> Type
type AggregateADT t = forall r. GGAggregate' 'Sum (ADTRep t) r


aggregateADT :: forall t. ConstructableADT t
  => AggregateADT t -> ADT t Expr -> ADT t Aggregate
aggregateADT f =
  ggaggregate' @'Sum @(ADTRep t) @(ADT t Expr) @(ADT t Aggregate) ADT (\(ADT a) -> a)
    (f @(ADT t Aggregate))


type AggregateHKD :: Type -> Type
type AggregateHKD a = forall r. GGAggregate' (GAlgebra (Rep a)) (HKDRep a) r


aggregateHKD :: forall a. ConstructableHKD a
  => AggregateHKD a -> HKD a Expr -> HKD a Aggregate
aggregateHKD f =
  ggaggregate' @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Expr) @(HKD a Aggregate) HKD (\(HKD a) -> a)
    (f @(HKD a Aggregate))
