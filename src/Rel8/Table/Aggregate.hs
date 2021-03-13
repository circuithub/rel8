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
import Rel8.Kind.Nullability ( withKnownNullability )
import Rel8.Schema.Context ( Aggregation( Aggregation ), DB( DB ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( hfield, hdicts, htabulate, hspecs )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Schema.Recontextualize ( Recontextualize )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Spec.ConstrainType ( ConstrainType )
import Rel8.Table ( Columns, toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.List ( ListTable(..) )
import Rel8.Table.NonEmpty ( NonEmptyTable(..) )
import Rel8.Type ( withDBType )
import Rel8.Type.Eq ( DBEq )


groupBy :: forall exprs aggregates.
  ( Recontextualize DB Aggregation exprs aggregates
  , EqTable exprs
  )
  => exprs -> aggregates
groupBy (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec _ nullability info -> case hfield dicts field of
      Dict -> case hfield exprs field of
        DB expr -> withKnownNullability nullability $ withDBType info $
          Aggregation $ groupByExpr expr
  where
    dicts = hdicts @(Columns exprs) @(ConstrainType DBEq)


listAgg :: forall exprs aggregates. ()
  => Recontextualize DB Aggregation exprs aggregates
  => exprs -> ListTable aggregates
listAgg (toColumns -> exprs) = ListTable $
  hvectorize
    (\(SSpec _ nullability info) (Identity (DB a)) ->
       withKnownNullability nullability $ withDBType info $
       Aggregation $ listAggExpr a)
    (Identity exprs)


nonEmptyAgg :: forall exprs aggregates. ()
  => Recontextualize DB Aggregation exprs aggregates
  => exprs -> NonEmptyTable aggregates
nonEmptyAgg (toColumns -> exprs) = NonEmptyTable $
  hvectorize
    (\(SSpec _ nullability info) (Identity (DB a)) ->
       withKnownNullability nullability $ withDBType info $
       Aggregation $ nonEmptyAggExpr a)
    (Identity exprs)
