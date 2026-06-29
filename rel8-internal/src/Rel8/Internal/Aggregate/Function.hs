{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Internal.Aggregate.Function (
  aggregateFunction,
  rawAggregateFunction,
) where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Internal.Aggregate (Aggregator1, unsafeMakeAggregator)
import Rel8.Internal.Aggregate.Fold (Fallback (Empty))
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Opaleye (castExpr, fromColumn, fromPrimExpr)
import Rel8.Internal.Schema.Null (Sql)
import Rel8.Internal.Schema.QualifiedName (QualifiedName, showQualifiedName)
import Rel8.Internal.Table (Table)
import Rel8.Internal.Table.Opaleye (unpackspec)
import Rel8.Internal.Type (DBType)


-- | 'aggregateFunction' allows the use use of custom aggregation functions
-- or PostgreSQL aggregation functions which are not otherwise supported by
-- Rel8.
aggregateFunction ::
  (Table Expr i, Sql DBType a) =>
  QualifiedName ->
  Aggregator1 i (Expr a)
aggregateFunction name = castExpr <$> rawAggregateFunction name


rawAggregateFunction :: Table Expr i => QualifiedName -> Aggregator1 i (Expr a)
rawAggregateFunction name =
  unsafeMakeAggregator
    id
    (fromPrimExpr . fromColumn)
    Empty
    (Opaleye.makeAggrExplicit unpackspec
      (Opaleye.AggrOther (showQualifiedName name)))
