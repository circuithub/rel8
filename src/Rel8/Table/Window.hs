{-# language MonoLocalBinds #-}

module Rel8.Table.Window
  ( cumulative
  , cumulative_
  , currentRow
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Window as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregates )
import qualified Rel8.Expr.Window as Expr
import Rel8.Schema.HTable ( hfield, htabulateA )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Window ( Window( Window ) )


-- | 'cumulative' allows the use of aggregation functions in 'Window'
-- expressions. In particular, @'cumulative' 'Rel8.sum'@
-- (when combined with 'Rel8.Window.orderPartitionBy') gives a running total,
-- also known as a \"cumulative sum\", hence the name @cumulative@.
cumulative :: Aggregates aggregates exprs => (a -> aggregates) -> Window a exprs
cumulative f =
  fmap fromColumns $
    htabulateA $ \field ->
      Expr.cumulative ((`hfield` field) . toColumns . f)


-- | A version of 'cumulative' for use with nullary aggregators like
-- 'Rel8.Expr.Aggregate.countStar'.
cumulative_ :: Aggregates aggregates exprs => aggregates -> Window a exprs
cumulative_ = cumulative . const


-- | Return every column of the current row of a window query.
currentRow :: Window a a
currentRow = Window $ Opaleye.over (Opaleye.noWindowFunction id) mempty mempty
