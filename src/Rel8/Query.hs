{-# language Arrows #-}
{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
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
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Profunctor ( Profunctor, Strong, Choice, Star(..) )
import Data.Profunctor ( lmap )
import Data.Profunctor.Traversing ( Traversing )
import Data.Tagged.PolyKinded ( Tagged(..) )
import qualified Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye ( PrimQuery, PrimQuery'(..), JoinType( LeftJoinLateral ) )
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
