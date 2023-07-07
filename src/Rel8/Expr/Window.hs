module Rel8.Expr.Window (
  cumulative,
  rowNumber,
  rank,
  denseRank,
  percentRank,
  cumeDist,
  ntile,
  lag,
  lagOn,
  lead,
  leadOn,
  firstValue,
  firstValueOn,
  lastValue,
  lastValueOn,
  nthValue,
  nthValueOn,
)
where

-- base
import Data.Int (Int32, Int64)
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Window as Opaleye
import qualified Opaleye.Window as Opaleye

-- profunctors
import Data.Profunctor (dimap, lmap)

-- rel8
import Rel8.Aggregate (Aggregator' (Aggregator))
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (fromColumn, fromPrimExpr, toColumn, toPrimExpr)
import Rel8.Schema.Null (Nullify)
import Rel8.Window (Window (Window))


{- | 'cumulative' allows the use of aggregation functions in 'Window'
expressions. In particular, @'cumulative' 'Rel8.sum'@
(when combined with 'Rel8.Window.orderPartitionBy') gives a running total,
also known as a \"cumulative sum\", hence the name @cumulative@.
-}
cumulative :: Aggregator' fold i a -> Window i a
cumulative f =
  fromWindowFunction $ Opaleye.aggregatorWindowFunction (fromAggregate f) id


-- | [@row_number()@](https://www.postgresql.org/docs/current/functions-window.html)
rowNumber :: Window i (Expr Int64)
rowNumber = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.rowNumber


-- | [@rank()@](https://www.postgresql.org/docs/current/functions-window.html)
rank :: Window i (Expr Int64)
rank = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.rank


-- | [@dense_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
denseRank :: Window i (Expr Int64)
denseRank = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.denseRank


-- | [@percent_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
percentRank :: Window i (Expr Double)
percentRank = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.percentRank


-- | [@cume_dist()@](https://www.postgresql.org/docs/current/functions-window.html)
cumeDist :: Window i (Expr Double)
cumeDist = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.cumeDist


-- | [@ntile(num_buckets)@](https://www.postgresql.org/docs/current/functions-window.html)
ntile :: Expr Int32 -> Window i (Expr Int32)
ntile buckets =
  fromWindowFunction $
    fromPrimExpr . fromColumn
      <$> Opaleye.ntile (toColumn (toPrimExpr buckets))


-- | [@lag(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lag :: Expr Int32 -> Expr a -> Window (Expr a) (Expr a)
lag offset def =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn) $
      Opaleye.lag (toColumn (toPrimExpr offset)) (toColumn (toPrimExpr def))


-- | Applies 'lag' to the column selected by the given function.
lagOn :: Expr Int32 -> Expr a -> (i -> Expr a) -> Window i (Expr a)
lagOn offset def f = lmap f (lag offset def)


-- | [@lead(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lead :: Expr Int32 -> Expr a -> Window (Expr a) (Expr a)
lead offset def =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn) $
      Opaleye.lead (toColumn (toPrimExpr offset)) (toColumn (toPrimExpr def))


-- | Applies 'lead' to the column selected by the given function.
leadOn :: Expr Int32 -> Expr a -> (i -> Expr a) -> Window i (Expr a)
leadOn offset def f = lmap f (lead offset def)


-- | [@first_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
firstValue :: Window (Expr a) (Expr a)
firstValue =
  fromWindowFunction $
    dimap
      (toColumn . toPrimExpr)
      (fromPrimExpr . fromColumn)
      Opaleye.firstValue


-- | Applies 'firstValue' to the column selected by the given function.
firstValueOn :: (i -> Expr a) -> Window i (Expr a)
firstValueOn f = lmap f firstValue


-- | [@last_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
lastValue :: Window (Expr a) (Expr a)
lastValue =
  fromWindowFunction $
    dimap
      (toColumn . toPrimExpr)
      (fromPrimExpr . fromColumn)
      Opaleye.lastValue


-- | Applies 'lastValue' to the column selected by the given function.
lastValueOn :: (i -> Expr a) -> Window i (Expr a)
lastValueOn f = lmap f lastValue


-- | [@nth_value(value, n)@](https://www.postgresql.org/docs/current/functions-window.html)
nthValue :: Expr Int32 -> Window (Expr a) (Expr (Nullify a))
nthValue n =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn) $
      Opaleye.nthValue (toColumn (toPrimExpr n))


-- | [@nth_value(value, n)@](https://www.postgresql.org/docs/current/functions-window.html)
nthValueOn :: Expr Int32 -> (i -> Expr a) -> Window i (Expr (Nullify a))
nthValueOn n f = lmap f (nthValue n)


fromAggregate :: Aggregator' fold i a -> Opaleye.Aggregator i a
fromAggregate (Aggregator _ a) = a


fromWindowFunction :: Opaleye.WindowFunction i a -> Window i a
fromWindowFunction (Opaleye.WindowFunction (Opaleye.PackMap w)) =
  Window $ Opaleye.Windows $ Opaleye.PackMap $ \f -> w $ \o -> f (o, mempty)
