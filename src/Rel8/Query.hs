{-# language Arrows #-}
{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TupleSections #-}

module Rel8.Query
  ( Query
  , toOpaleye
  , unpackspec
  , each
  , where_
  , catMaybe_
  , limit
  , offset
  , union
  , unionAll
  , intersect
  , intersectAll
  , except
  , exceptAll
  , distinct
  , optional
  , filter
  , filterA
  , values
  ) where

-- base
import Control.Arrow ( Arrow, ArrowChoice, Kleisli( Kleisli ), returnA )
import Control.Category ( Category )
import Data.Coerce
import Data.Functor.Compose ( Compose( Compose ) )
import Prelude hiding ( filter )
import Numeric.Natural ( Natural )

-- opaleye
import Opaleye
  ( exceptAllExplicit
  , exceptExplicit
  , intersectAllExplicit
  , intersectExplicit
  , restrict
  , selectTableExplicit
  , unionAllExplicit
  , unionExplicit
  , valuesExplicit
  )
import qualified Opaleye
  ( limit
  , offset
  )
import Opaleye.Internal.Aggregate ( Aggregator( Aggregator ) )
import Opaleye.Internal.Binary ( Binaryspec( Binaryspec ) )
import Opaleye.Internal.Distinct ( Distinctspec( Distinctspec ), distinctExplicit )
import Opaleye.Internal.PackMap ( PackMap( PackMap ), extractAttr, run )
import Opaleye.Internal.PrimQuery
  ( JoinType( LeftJoinLateral )
  , PrimQuery
  , PrimQuery'( Join, Unit )
  )
import Opaleye.Internal.QueryArr ( QueryArr( QueryArr ) )
import Opaleye.Internal.Tag ( Tag, next )
import Opaleye.Internal.Unpackspec ( Unpackspec( Unpackspec ), runUnpackspec )
import Opaleye.Internal.Values ( Valuesspec( Valuesspec ) )

-- profunctors
import Data.Profunctor ( Choice, Profunctor, Star( Star ), Strong, lmap )
import Data.Profunctor.Traversing ( Traversing )

-- rel8
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose( HCompose ) )
import Data.Indexed.Functor.Identity ( unHIdentity )
import Data.Indexed.Functor.Product ( HProduct( HProduct ) )
import Rel8.Column
  ( Column( Column )
  , fromJust
  , toOpaleyeColumn
  , toPrimExpr
  , traversePrimExpr
  )
import qualified Rel8.Column ( zipColumnsM )
import Rel8.Row
  ( MaybeRow( MaybeRow )
  , Row( Row )
  , lit
  , sequenceColumns
  , toColumns
  , traverseColumns
  , zipColumnsM
  )
import Rel8.Schema ( TableSchema, table )
import Rel8.Table ( Table )

-- transformers
import Control.Monad.Trans.State.Strict ( State, runState, state )


newtype Query a b =
  Query (Star (State QueryState) a b)
  deriving (Functor, Applicative, Category, Profunctor, Strong, Choice, Traversing)
  deriving (Arrow, ArrowChoice) via Kleisli (State QueryState)


each :: Table a => TableSchema a -> Query x (Row a)
each = generalise . fromOpaleye . selectTableExplicit unpackspec . table


unpackspec :: Table a => Unpackspec (Row a) (Row a)
unpackspec =
  Unpackspec $ PackMap \f -> traverseColumns (traversePrimExpr f)


optional :: Query a b -> Query a (MaybeRow b)
optional query = fromOpaleye $ QueryArr arrow
  where
    arrow (a, left, t1) = (maybeB, join, next tag')
      where
        join =
          Join LeftJoinLateral true [] bindings left right

        ((t2, b), right, tag') = f (a, Unit, t1)
          where
            QueryArr f = (,) <$> pure (lit False) <*> toOpaleye query

        (t3, bindings) =
          run
            ( runUnpackspec
                unpackspec
                ( extractAttr "maybe" tag' )
                t2
            )

        maybeB =
          MaybeRow t3 b

    true = toPrimExpr $ unHIdentity $ toColumns $ lit True


where_ :: Query (Row Bool) ()
where_ =
  fromOpaleye $ lmap (toOpaleyeColumn . unHIdentity . toColumns) restrict


catMaybe_ :: Table b => Query a (Row (Maybe b)) -> Query a (Row b)
catMaybe_ q = proc a -> do
  Row (HProduct isNull (HCompose b)) <- q -< a
  where_ -< Row isNull
  returnA -< Row $ hmap (coerce fromJust) b


data QueryState =
  QueryState PrimQuery Tag


toOpaleye :: Query a b -> QueryArr a b
toOpaleye (Query (Star m)) =
  QueryArr \(a, pq, t0) -> out (runState (m a) (QueryState pq t0))
  where
    out (b, QueryState pq t) = (b, pq, t)


fromOpaleye :: QueryArr a b -> Query a b
fromOpaleye (QueryArr f) =
  Query $ Star $ \a -> state \(QueryState pq t) -> out (f (a, pq, t))
  where
    out (b, pq, t) = (b, QueryState pq t)


liftOpaleye :: (QueryArr s t -> QueryArr a b) -> Query s t -> Query a b
liftOpaleye f = fromOpaleye . f . toOpaleye


liftOpaleye2
  :: ( QueryArr a1 b1 -> QueryArr a2 b2 -> QueryArr a3 b3 )
  -> Query a1 b1
  -> Query a2 b2
  -> Query a3 b3
liftOpaleye2 f x y = fromOpaleye $ f (toOpaleye x) (toOpaleye y)


limit :: Natural -> Query () a -> Query x a
limit n = liftOpaleye (generalise . Opaleye.limit (fromIntegral n))


offset :: Natural -> Query () a -> Query x a
offset n = liftOpaleye (generalise . Opaleye.offset (fromIntegral n))


union :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
union x y = generalise $ liftOpaleye2 (unionExplicit binaryspec) x y


unionAll :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
unionAll x y = generalise $ liftOpaleye2 (unionAllExplicit binaryspec) x y


intersect :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
intersect x y = generalise $ liftOpaleye2 (intersectExplicit binaryspec) x y


intersectAll :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
intersectAll x y = generalise $ liftOpaleye2 (intersectAllExplicit binaryspec) x y


except :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
except x y = generalise $ liftOpaleye2 (exceptExplicit binaryspec) x y


exceptAll :: Table a => Query () (Row a) -> Query () (Row a) -> Query x (Row a)
exceptAll x y = generalise $ liftOpaleye2 (exceptAllExplicit binaryspec) x y


binaryspec :: Table a => Binaryspec (Row a) (Row a)
binaryspec = Binaryspec $ PackMap \f -> uncurry (zipColumnsM (Rel8.Column.zipColumnsM (curry f)))


distinct :: Table a => Query () (Row a) -> Query x (Row a)
distinct = liftOpaleye (generalise . distinctExplicit distinctspec)


distinctspec :: Table a => Distinctspec (Row a) (Row a)
distinctspec = Distinctspec $ Aggregator $ PackMap \f -> traverseColumns (traversePrimExpr (f . (Nothing,)))


values :: (Foldable f, Table a) => f a -> Query x (Row a)
values = generalise . fromOpaleye . valuesExplicit unpackspec valuesspec . foldMap (pure . lit)


valuesspec :: Table a => Valuesspec (Row a) (Row a)
valuesspec = Valuesspec $ PackMap \f () -> sequenceColumns (Column <$> f ())


generalise :: Profunctor p => p () b -> p a b
generalise = lmap mempty


filter :: (a -> Row Bool) -> Query a a
filter f = lmap (f ,) $ filterA returnA


filterA :: Query i a -> Query (a -> Row Bool, i) a
filterA query = proc (f, i) -> do
  a <- query -< i
  where_ -< f a
  returnA -< a
