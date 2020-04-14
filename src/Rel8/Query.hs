{-# language Arrows #-}
{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}

module Rel8.Query where

import Control.Applicative ( Const(..) )
import Control.Arrow ( Arrow, ArrowChoice, Kleisli(..), returnA )
import Control.Category ( Category )
import Control.Monad.Trans.State.Strict ( State, runState, state )
import Data.Coerce
import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..), hsequence )
import Data.Profunctor ( Profunctor, Strong, Choice, Star(..) )
import Data.Profunctor ( lmap )
import Data.Profunctor.Traversing ( Traversing )
import Data.Tagged.PolyKinded ( Tagged(..) )
import Numeric.Natural ( Natural )
import qualified Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye ( PrimQuery, PrimQuery'(..), JoinType(..) )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import Rel8.Expr
import Rel8.Schema
import Rel8.Table


newtype Query a b =
  Query (Star (State QueryState) a b)
  deriving (Functor, Applicative, Category, Profunctor, Strong, Choice, Traversing)
  deriving (Arrow, ArrowChoice) via Kleisli (State QueryState)


runQuery :: a -> Query a b -> (b, QueryState)
runQuery a q = runState (coerce q a) emptyQueryState


each :: Table a => Schema a -> Query x (Expr a)
each Schema{ tableName, schema = Columns columnNames } =
  lmap (const ()) $
  fromOpaleye $
  Opaleye.selectTableExplicit unpackspec $
  Opaleye.Table tableName $
  Opaleye.TableProperties
    (Opaleye.Writer @() $ Opaleye.PackMap \_ _ -> pure ())
    (Opaleye.View $ Expr $ htabulate \i -> Const $ Opaleye.BaseTableAttrExpr $ getConst $ hindex columnNames i)


unpackspec :: HTraversable (Pattern a) => Opaleye.Unpackspec (Expr a) (Expr a)
unpackspec = Opaleye.Unpackspec $ Opaleye.PackMap \f (Expr x) -> fmap Expr $ htraverse (fmap Const . f . getConst) x


optional :: Table b => Query a (Expr b) -> Query a (Expr (Maybe b))
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
          Expr $ Compose $ Tagged $ HProduct (toPrimExprs t') (HCompose (hmap (Compose . Const . getConst) (toPrimExprs b)))

    true =
      case lit True of Expr (HIdentity (Const prim)) -> prim


where_ :: Query (Expr Bool) ()
where_ =
  fromOpaleye $ lmap (\(Expr (HIdentity (Const prim))) -> Opaleye.Column prim) Opaleye.restrict


catMaybe_ :: Table b => Query a (Expr (Maybe b)) -> Query a (Expr b)
catMaybe_ q = proc a -> do
  Expr (Compose (Tagged (HProduct isNull (HCompose row)))) <- q -< a
  where_ -< Expr $ isNull
  returnA -< Expr $ hmap (\(Compose (Const x)) -> Const x) row


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
limit n = lmap (const ()) . fromOpaleye . Opaleye.limit ( fromIntegral n ) . toOpaleye . lmap (const ())


offset :: Natural -> Query () a -> Query x a
offset n = lmap (const ()) . fromOpaleye . Opaleye.offset ( fromIntegral n ) . toOpaleye . lmap (const ())


leftJoin :: Table b => (forall a. Query a (Expr b)) -> Query (Expr b -> Expr Bool) (Expr (Maybe b))
leftJoin query = fromOpaleye $ Opaleye.QueryArr arrow
  where
    arrow (f, left, tag) = (maybeB, join, Opaleye.next tag')
      where
        join =
          Opaleye.Join Opaleye.LeftJoin
            (boolPrimExpr (f b))
            []
            bindings
            left
            right

        ((t, b), right, tag') = inner ((), Opaleye.Unit, tag)
          where
            Opaleye.QueryArr inner = (,) <$> pure (lit False) <*> toOpaleye query

        (t', bindings) =
          Opaleye.run
            ( Opaleye.runUnpackspec
                unpackspec
                ( Opaleye.extractAttr "maybe" tag' )
                t
            )

        maybeB =
          Expr $ Compose $ Tagged $ HProduct (toPrimExprs t') (HCompose (hmap (Compose . Const . getConst) (toPrimExprs b)))

    boolPrimExpr :: Expr Bool -> Opaleye.PrimExpr
    boolPrimExpr = coerce


union :: Table a => Query () (Expr a) -> Query () (Expr a) -> Query () (Expr a)
union x y = lmap (const ()) $ fromOpaleye $ Opaleye.unionExplicit binaryspec (toOpaleye x) (toOpaleye y)


unionAll :: Table a => Query () (Expr a) -> Query () (Expr a) -> Query () (Expr a)
unionAll x y = lmap (const ()) $ fromOpaleye $ Opaleye.unionAllExplicit binaryspec (toOpaleye x) (toOpaleye y)


intersect :: Table a => Query () (Expr a) -> Query () (Expr a) -> Query () (Expr a)
intersect x y = lmap (const ()) $ fromOpaleye $ Opaleye.intersectExplicit binaryspec (toOpaleye x) (toOpaleye y)


intersectAll :: Table a => Query () (Expr a) -> Query () (Expr a) -> Query () (Expr a)
intersectAll x y = lmap (const ()) $ fromOpaleye $ Opaleye.intersectAllExplicit binaryspec (toOpaleye x) (toOpaleye y)


except :: Table a => Query () (Expr a) -> Query () (Expr a) -> Query () (Expr a)
except x y = lmap (const ()) $ fromOpaleye $ Opaleye.exceptExplicit binaryspec (toOpaleye x) (toOpaleye y)


exceptAll :: Table a => Query () (Expr a) -> Query () (Expr a) -> Query () (Expr a)
exceptAll x y = lmap (const ()) $ fromOpaleye $ Opaleye.exceptAllExplicit binaryspec (toOpaleye x) (toOpaleye y)


binaryspec :: Table a => Opaleye.Binaryspec (Expr a) (Expr a)
binaryspec = Opaleye.Binaryspec $ Opaleye.PackMap \f (Expr l, Expr r) -> fmap Expr $ hsequence $ htabulate \i -> Compose $ Const <$> f (getConst $ hindex l i, getConst $ hindex r i)
