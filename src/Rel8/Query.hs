{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}

module Rel8.Query where

import Control.Arrow ( Arrow, ArrowChoice, Kleisli(..) )
import Control.Category ( Category )
import Control.Monad.Trans.State.Strict ( State, runState )
import Data.Coerce
import Data.Profunctor ( Profunctor, Strong, Choice, Star(..) )
import Data.Profunctor.Traversing ( Traversing )
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import Rel8.Expr
import Rel8.Schema


newtype Query a b =
  Query (Star (State QueryState) a b)
  deriving (Functor, Applicative, Category, Profunctor, Strong, Choice, Traversing)
  deriving (Arrow, ArrowChoice) via Kleisli (State QueryState)


runQuery :: a -> Query a b -> (b, QueryState)
runQuery a q = runState (coerce q a) emptyQueryState


each :: Schema a -> Query x (Expr a)
each = undefined


optional :: Query a (Expr b) -> Query a (Expr (Maybe b))
optional = undefined


where_ :: Query Bool ()
where_ = undefined


catMaybe_ :: Query a (Expr (Maybe b)) -> Query a (Expr b)
catMaybe_ = undefined


data QueryState =
  QueryState
    { primQuery :: Opaleye.PrimQuery
    , tag :: Opaleye.Tag
    }


emptyQueryState :: QueryState
emptyQueryState =
  QueryState { primQuery = Opaleye.Unit, tag = Opaleye.start }
