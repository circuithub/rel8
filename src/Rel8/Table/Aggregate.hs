{-# language FlexibleContexts #-}
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
import Data.Functor.Identity ( Identity( Identity ) )
import Prelude

-- rel8
import Rel8.Expr.Aggregate ( groupByExpr, listAggExpr, nonEmptyAggExpr )
import Rel8.Schema.Context ( Aggregation( Aggregation ), DB( DB ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( hfield, hdicts, htabulate )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Columns, toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.List ( ListTable(..) )
import Rel8.Table.NonEmpty ( NonEmptyTable(..) )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type.Eq ( DBEq )


groupBy :: forall exprs aggregates.
  ( Recontextualize DB Aggregation exprs aggregates
  , EqTable exprs
  )
  => exprs -> aggregates
groupBy (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield dicts field of
    Dict -> case hfield exprs field of
      DB expr -> Aggregation $ groupByExpr expr
  where
    dicts = hdicts @(Columns exprs) @(ConstrainDBType DBEq)


listAgg :: forall exprs aggregates. ()
  => Recontextualize DB Aggregation exprs aggregates
  => exprs -> ListTable aggregates
listAgg (toColumns -> exprs) = ListTable $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ listAggExpr a)
    (pure exprs)


nonEmptyAgg :: forall exprs aggregates. ()
  => Recontextualize DB Aggregation exprs aggregates
  => exprs -> NonEmptyTable aggregates
nonEmptyAgg (toColumns -> exprs) = NonEmptyTable $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ nonEmptyAggExpr a)
    (pure exprs)
