{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wno-redundant-constraints #-}

module Rel8.Aggregate
  ( Aggregate(..)
  , aggregate
  , groupBy
  , boolAnd
  , boolOr
  , count
  , countStar
  , countDistinct
  , arrayAgg
  , listAgg
  , nonEmptyArrayAgg
  , nonEmptyAgg
  , some
  , many
  , aggregateAllExprs
  , sequenceAggregate
  ) where

-- base
import Data.Int ( Int64 )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )

-- rel8
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import Rel8.Context ( Context( Column ), Meta )
import Rel8.DBType.DBEq ( DBEq )
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Instances ( Column( ExprColumn ) )
import Rel8.Expr.Opaleye ( mapPrimExpr )
import Rel8.HTable ( HField, HTable, hfield, htabulate )
import Rel8.HTable.HMapTable ( HMapTable, HMapTableField( HMapTableField ) )
import Rel8.Query ( Query, mapOpaleye )
import Rel8.Table ( AllColumns, Columns, Table( toColumns ) )
import Rel8.Table.Congruent ( Congruent, traverseTable )
import Rel8.Table.ListTable ( ListOf, ListTable( ListTable ) )
import Rel8.Table.MaybeTable ( maybeTable, optional )
import Rel8.Table.NonEmptyTable ( NonEmptyList, NonEmptyTable( NonEmptyTable ) )

-- semigroupoids
import Data.Functor.Apply ( Apply, WrappedApplicative( WrapApplicative, unwrapApplicative ) )


-- | An @Aggregate a@ describes how to aggregate @Table@s of type @a@. You can
-- unpack an @Aggregate@ back to @a@ by running it with 'aggregate'. As
-- @Aggregate@ is an 'Applicative' functor, you can combine @Aggregate@s using
-- the normal @Applicative@ combinators, or by working in @do@ notation with
-- @ApplicativeDo@.
newtype Aggregate a = Aggregate (Opaleye.Aggregator () a)
  deriving (Functor, Apply) via WrappedApplicative (Opaleye.Aggregator ())


instance Context Aggregate where
  newtype Column Aggregate :: Meta -> Type where
    AggregateColumn :: { fromAggregateColumn :: Aggregate (Column Expr a) } -> Column Aggregate a


-- | Apply an aggregation to all rows returned by a 'Query'.
aggregate :: forall a. Table Expr a => Query (Aggregate a) -> Query a
aggregate = mapOpaleye $ Opaleye.aggregate aggregator
  where
    aggregator :: Opaleye.Aggregator (Aggregate a) a
    aggregator =
      Opaleye.Aggregator $
        Opaleye.PackMap \f (Aggregate (Opaleye.Aggregator (Opaleye.PackMap g))) ->
          g f ()


-- | Aggregate a value by grouping by it. @groupBy@ is just a synonym for
-- 'pure', but sometimes being explicit can help the readability of your code.
groupBy :: (Table Expr a, AllColumns a DBEq) => a -> Aggregate a
groupBy x = Aggregate $ Opaleye.Aggregator $ Opaleye.PackMap \f () ->
  unwrapApplicative $
    traverseTable (\(ExprColumn (Expr e)) -> ExprColumn . Expr <$> WrapApplicative (f (Nothing, e))) x


aggregateAllExprs :: Opaleye.AggrOp -> Expr a -> Aggregate (Expr b)
aggregateAllExprs = aggregateSomeExprs Opaleye.AggrAll


aggregateDistinctExprs :: Opaleye.AggrOp -> Expr a -> Aggregate (Expr b)
aggregateDistinctExprs = aggregateSomeExprs Opaleye.AggrDistinct


aggregateSomeExprs :: Opaleye.AggrDistinct -> Opaleye.AggrOp -> Expr a -> Aggregate (Expr b)
aggregateSomeExprs which op (Expr a) =
  Aggregate $
    Opaleye.Aggregator $
      Opaleye.PackMap \f () ->
        Expr <$> f (Just (op, [], which), a)


-- | Corresponds to @bool_and@.
boolAnd :: Expr Bool -> Aggregate (Expr Bool)
boolAnd = aggregateAllExprs Opaleye.AggrBoolAnd


-- | Corresponds to @bool_or@.
boolOr :: Expr Bool -> Aggregate (Expr Bool)
boolOr = aggregateAllExprs Opaleye.AggrBoolOr


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
count :: Expr a -> Aggregate (Expr Int64)
count = aggregateAllExprs Opaleye.AggrCount


-- | Corresponds to @COUNT(*)@.
countStar :: Aggregate (Expr Int64)
countStar = count (0 :: Expr Int64)


-- | Count the number of distinct occurances of a single column. Corresponds to
-- @COUNT(DISTINCT a)@
countDistinct :: Expr a -> Aggregate (Expr Int64)
countDistinct = aggregateDistinctExprs Opaleye.AggrCount


arrayAgg :: Expr a -> Aggregate (Expr [a])
arrayAgg = fmap wrapInRow . aggregateAllExprs Opaleye.AggrArr


nonEmptyArrayAgg :: Expr a -> Aggregate (Expr (NonEmpty a))
nonEmptyArrayAgg = fmap wrapInRow . aggregateAllExprs Opaleye.AggrArr


wrapInRow :: Expr a -> Expr a
wrapInRow = mapPrimExpr (Opaleye.FunExpr "row" . pure)


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
listAgg :: forall exprs. Table Expr exprs => exprs -> Aggregate (ListTable exprs)
listAgg exprs = fmap ListTable $ sequenceAggregate $ htabulate f
  where
    f :: forall x. HField (HMapTable ListOf (Columns exprs)) x -> Column Aggregate x
    f (HMapTableField i) =
      case hfield (toColumns exprs) i of
        ExprColumn e -> AggregateColumn $ ExprColumn <$> arrayAgg e


-- | Like 'listAgg', but the result is guaranteed to be a non-empty list.
nonEmptyAgg :: forall exprs. Table Expr exprs => exprs -> Aggregate (NonEmptyTable exprs)
nonEmptyAgg exprs = fmap NonEmptyTable $ sequenceAggregate $ htabulate f
  where
    f :: forall x. HField (HMapTable NonEmptyList (Columns exprs)) x -> Column Aggregate x
    f (HMapTableField i) =
      case hfield (toColumns exprs) i of
        ExprColumn e -> AggregateColumn $ ExprColumn <$> nonEmptyArrayAgg e


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


class (Table Aggregate aggregates, Table Expr exprs, Congruent aggregates exprs) => SequenceAggregate aggregates exprs | aggregates -> exprs


instance (HTable c, c ~ c', a ~ Column Aggregate, a' ~ Column Expr) => SequenceAggregate (c a) (c' a')


sequenceAggregate :: SequenceAggregate aggregates exprs => aggregates -> Aggregate exprs
sequenceAggregate = traverseTable fromAggregateColumn
