{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Aggregate.Function (
  aggregateFunction,
) where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate (Aggregator1, unsafeMakeAggregator)
import Rel8.Aggregate.Fold (Fallback (Empty))
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (castExpr, fromColumn, fromPrimExpr)
import Rel8.Schema.Null (Sql)
import Rel8.Schema.QualifiedName (QualifiedName, showQualifiedName)
import Rel8.Table (Table)
import Rel8.Table.Opaleye (unpackspec)
import Rel8.Type (DBType)


-- | 'aggregateFunction' allows the use use of custom aggregation functions
-- or PostgreSQL aggregation functions which are not otherwise supported by
-- Rel8.
aggregateFunction ::
  (Table Expr i, Sql DBType a) =>
  QualifiedName ->
  Aggregator1 i (Expr a)
aggregateFunction name =
  unsafeMakeAggregator
    id
    (castExpr . fromPrimExpr . fromColumn)
    Empty
    (Opaleye.makeAggrExplicit unpackspec
      (Opaleye.AggrOther (showQualifiedName name)))
