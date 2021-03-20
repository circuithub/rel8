{-# language FlexibleContexts #-}
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
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr.Aggregate ( groupByExpr, slistAggExpr, snonEmptyAggExpr )
import Rel8.Schema.Context ( Aggregation( Aggregation ), DB( DB ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( hfield, hdicts, htabulate )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Table, Columns, toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Map ( MapTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Type.Eq ( DBEq )


groupBy :: forall exprs. EqTable exprs => exprs -> Aggregate exprs
groupBy (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield dicts field of
    Dict -> case hfield exprs field of
      DB expr -> Aggregation $ groupByExpr expr
  where
    dicts = hdicts @(Columns exprs) @(ConstrainDBType DBEq)


listAgg :: Table DB exprs => exprs -> Aggregate (ListTable exprs)
listAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\(SSpec _ _ nullability blueprint) (Identity (DB a)) ->
       Aggregation $ slistAggExpr nullability blueprint a)
    (pure exprs)


nonEmptyAgg :: Table DB exprs => exprs -> Aggregate (NonEmptyTable exprs)
nonEmptyAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\(SSpec _ _ nullability blueprint) (Identity (DB a)) ->
       Aggregation $ snonEmptyAggExpr nullability blueprint a)
    (pure exprs)


runAggregation :: MapTable Aggregation DB aggregates exprs
  => aggregates -> Aggregate exprs
runAggregation = fromColumns . toColumns
