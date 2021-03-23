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
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr.Aggregate ( listAggExpr, nonEmptyAggExpr, sgroupByExpr )
import Rel8.Schema.Context ( Aggregation( Aggregation ), DB( DB ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( htabulate, hfield, hdicts, hspecs )
import Rel8.Schema.HTable.Vectorize ( hvectorize )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Table, Columns, toColumns, fromColumns )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.Recontextualize ( Aggregates )
import Rel8.Type.Eq ( DBEq )


groupBy :: forall exprs. EqTable exprs => exprs -> Aggregate exprs
groupBy (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield dicts field of
    Dict -> case hfield hspecs field of
      SSpec {nullability} -> case hfield exprs field of
        DB expr -> Aggregation $ sgroupByExpr nullability expr
  where
    dicts = hdicts @(Columns exprs) @(ConstrainDBType DBEq)


listAgg :: Table DB exprs => exprs -> Aggregate (ListTable exprs)
listAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ listAggExpr a)
    (pure exprs)


nonEmptyAgg :: Table DB exprs => exprs -> Aggregate (NonEmptyTable exprs)
nonEmptyAgg (toColumns -> exprs) = fromColumns $
  hvectorize
    (\_ (Identity (DB a)) -> Aggregation $ nonEmptyAggExpr a)
    (pure exprs)


runAggregation :: Aggregates aggregates exprs => aggregates -> Aggregate exprs
runAggregation = fromColumns . toColumns
