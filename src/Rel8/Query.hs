{-# language Arrows #-}
{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}

module Rel8.Query where

import Control.Arrow ( Arrow, ArrowChoice, Kleisli(..), returnA )
import Control.Category ( Category )
import Control.Monad.Trans.State.Strict ( State, runState, state )
import Data.Coerce
import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Profunctor ( Profunctor, Strong, Choice, Star(..) )
import Data.Profunctor ( lmap )
import Data.Profunctor.Traversing ( Traversing )
import Data.Tagged.PolyKinded ( Tagged(..) )
import Numeric.Natural ( Natural )
import qualified Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye ( PrimQuery, PrimQuery'(..), JoinType(..) )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Rel8.Column as Column
import Rel8.Row
import Rel8.Schema
import Rel8.Table


newtype Query a b =
  Query (Star (State QueryState) a b)
  deriving (Functor, Applicative, Category, Profunctor, Strong, Choice, Traversing)
  deriving (Arrow, ArrowChoice) via Kleisli (State QueryState)


runQuery :: a -> Query a b -> (b, QueryState)
runQuery a q =
  runState (coerce q a) emptyQueryState


each :: Table a => TableSchema a -> Query x (Row a)
each = generalise . fromOpaleye . Opaleye.selectTableExplicit unpackspec . table


unpackspec :: Table a => Opaleye.Unpackspec (Row a) (Row a)
unpackspec =
  Opaleye.Unpackspec $ Opaleye.PackMap \f -> traverseColumns (Column.traversePrimExpr f)


optional :: Table b => Query a (Row b) -> Query a (Row (Maybe b))
optional query = fromOpaleye $ Opaleye.QueryArr arrow
  where
    arrow (a, left, tag) = (maybeB, join, Opaleye.next tag')
      where
        join =
          Opaleye.Join Opaleye.LeftJoinLateral true [] bindings left right

        ((t, b), right, tag') = f (a, Opaleye.Unit, tag)
          where
            Opaleye.QueryArr f = (,) <$> pure (lit False) <*> toOpaleye query

        (t', bindings) =
          Opaleye.run
            ( Opaleye.runUnpackspec
                unpackspec
                ( Opaleye.extractAttr "maybe" tag' )
                t
            )

        maybeB =
          Row $ Compose $ Tagged $
          HProduct
            (toColumns t')
            (HCompose (hmap (coerce Column.just) (toColumns b)))

    true = Column.toPrimExpr $ Column.lit $ Opaleye.BoolLit True


where_ :: Query (Row Bool) ()
where_ =
  fromOpaleye $ lmap (Column.toOpaleyeColumn . unHIdentity . toColumns) Opaleye.restrict


catMaybe_ :: Table b => Query a (Row (Maybe b)) -> Query a (Row b)
catMaybe_ q = proc a -> do
  Row (Compose (Tagged (HProduct isNull (HCompose row)))) <- q -< a
  where_ -< Row $ isNull
  returnA -< Row $ hmap (coerce Column.fromJust) row


data QueryState =
  QueryState
    { primQuery :: Opaleye.PrimQuery
    , tag :: Opaleye.Tag
    }


emptyQueryState :: QueryState
emptyQueryState =
  QueryState { primQuery = Opaleye.Unit, tag = Opaleye.start }


toOpaleye :: Query a b -> Opaleye.QueryArr a b
toOpaleye (Query (Star m)) =
  Opaleye.QueryArr \(a, pq, t0) -> out (runState (m a) (QueryState pq t0))
  where
    out (b, QueryState pq t) = (b, pq, t)


fromOpaleye :: Opaleye.QueryArr a b -> Query a b
fromOpaleye (Opaleye.QueryArr f) =
  Query $ Star $ \a -> state \(QueryState pq t) -> out (f (a, pq, t))
  where
    out (b, pq, t) = (b, QueryState pq t)


limit :: Natural -> Query () a -> Query x a
limit n = generalise . fromOpaleye . Opaleye.limit (fromIntegral n) . toOpaleye . generalise


offset :: Natural -> Query () a -> Query x a
offset n = generalise . fromOpaleye . Opaleye.offset (fromIntegral n) . toOpaleye . generalise


union :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
union x y = generalise $ fromOpaleye $ Opaleye.unionExplicit binaryspec (toOpaleye x) (toOpaleye y)


unionAll :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
unionAll x y = generalise $ fromOpaleye $ Opaleye.unionAllExplicit binaryspec (toOpaleye x) (toOpaleye y)


intersect :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
intersect x y = generalise $ fromOpaleye $ Opaleye.intersectExplicit binaryspec (toOpaleye x) (toOpaleye y)


intersectAll :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
intersectAll x y = generalise $ fromOpaleye $ Opaleye.intersectAllExplicit binaryspec (toOpaleye x) (toOpaleye y)


except :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
except x y = generalise $ fromOpaleye $ Opaleye.exceptExplicit binaryspec (toOpaleye x) (toOpaleye y)


exceptAll :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
exceptAll x y = generalise $ fromOpaleye $ Opaleye.exceptAllExplicit binaryspec (toOpaleye x) (toOpaleye y)


binaryspec :: Table a => Opaleye.Binaryspec (Row a) (Row a)
binaryspec = Opaleye.Binaryspec $ Opaleye.PackMap \f -> uncurry (zipColumnsM (Column.zipColumnsM (curry f)))


distinct :: Table a => Query () (Row a) -> Query x (Row a)
distinct = generalise . fromOpaleye . Opaleye.distinctExplicit distinctspec . toOpaleye


distinctspec :: Table a => Opaleye.Distinctspec (Row a) (Row a)
distinctspec = Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f -> traverseColumns (Column.traversePrimExpr (f . (Nothing,)))


values :: (Foldable f, Table a) => f a -> Query x (Row a)
values = generalise . fromOpaleye . Opaleye.valuesExplicit unpackspec valuesspec . foldMap (pure . lit)


valuesspec :: Table a => Opaleye.Valuesspec (Row a) (Row a)
valuesspec = Opaleye.Valuesspec $ Opaleye.PackMap \f () -> sequenceColumns (Column.Column <$> f ())


generalise :: Profunctor p => p () b -> p a b
generalise = lmap mempty
