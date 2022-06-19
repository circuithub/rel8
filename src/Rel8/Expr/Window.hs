module Rel8.Expr.Window
  ( cumulative
  , rowNumber
  , rank
  , denseRank
  , percentRank
  , cumeDist
  , ntile
  , lag
  , lead
  , firstValue
  , lastValue
  , nthValue
  )
where

-- base
import Data.Int ( Int32, Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Window as Opaleye
import qualified Opaleye.Window as Opaleye

-- profunctors
import Data.Profunctor (dimap)

-- rel8
import Rel8.Aggregate ( Aggregate( Aggregate ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromColumn, fromPrimExpr, toColumn, toPrimExpr )
import Rel8.Schema.Null ( Nullify )
import Rel8.Window ( Window( Window ) )


cumulative :: (a -> Aggregate b) -> Window a (Expr b)
cumulative f =
  fromWindowFunction $ Opaleye.aggregatorWindowFunction (fromAggregate f) id


-- | [@row_number()@](https://www.postgresql.org/docs/current/functions-window.html)
rowNumber :: Window a (Expr Int64)
rowNumber = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.rowNumber


-- | [@rank()@](https://www.postgresql.org/docs/current/functions-window.html)
rank :: Window a (Expr Int64)
rank = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.rank


-- | [@dense_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
denseRank :: Window a (Expr Int64)
denseRank = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.denseRank


-- | [@percent_rank()@](https://www.postgresql.org/docs/current/functions-window.html)
percentRank :: Window a (Expr Double)
percentRank = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.percentRank


-- | [@cume_dist()@](https://www.postgresql.org/docs/current/functions-window.html)
cumeDist :: Window a (Expr Double)
cumeDist = fromWindowFunction $ fromPrimExpr . fromColumn <$> Opaleye.cumeDist


-- | [@ntile(num_buckets)@](https://www.postgresql.org/docs/current/functions-window.html)
ntile :: Expr Int32 -> Window a (Expr Int32)
ntile buckets = fromWindowFunction $ fromPrimExpr . fromColumn <$>
  Opaleye.ntile (toColumn (toPrimExpr buckets))


-- | [@lag(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lag :: Expr Int32 -> Expr a -> Window (Expr a) (Expr a)
lag offset def =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn) $
      Opaleye.lag (toColumn (toPrimExpr offset)) (toColumn (toPrimExpr def))


-- | [@lead(value, offset, default)@](https://www.postgresql.org/docs/current/functions-window.html)
lead :: Expr Int32 -> Expr a -> Window (Expr a) (Expr a)
lead offset def =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn) $
      Opaleye.lead (toColumn (toPrimExpr offset)) (toColumn (toPrimExpr def))


-- | [@first_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
firstValue :: Window (Expr a) (Expr a)
firstValue =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn)
      Opaleye.firstValue


-- | [@last_value(value)@](https://www.postgresql.org/docs/current/functions-window.html)
lastValue :: Window (Expr a) (Expr a)
lastValue =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn)
      Opaleye.lastValue


-- | [@nth_value(value, n)@](https://www.postgresql.org/docs/current/functions-window.html)
nthValue :: Expr Int32 -> Window (Expr a) (Expr (Nullify a))
nthValue n =
  fromWindowFunction $
    dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn) $
      Opaleye.nthValue (toColumn (toPrimExpr n))


fromAggregate :: (a -> Aggregate b) -> Opaleye.Aggregator a (Expr b)
fromAggregate f = Opaleye.Aggregator $ Opaleye.PackMap $ \w a -> case f a of
  Aggregate (Opaleye.Aggregator (Opaleye.PackMap x)) -> x w ()


fromWindowFunction :: Opaleye.WindowFunction a b -> Window a b
fromWindowFunction (Opaleye.WindowFunction (Opaleye.PackMap w)) =
  Window $ Opaleye.Windows $ Opaleye.PackMap $ \f -> w $ \o -> f (o, mempty)
