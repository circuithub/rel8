{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}

module Rel8.Query where

import Control.Arrow ( Arrow, ArrowChoice, Kleisli(..) )
import Control.Category ( Category )
import Control.Monad.Trans.State.Strict ( State )
import Data.Profunctor ( Profunctor, Strong, Choice, Star(..) )
import Data.Profunctor.Traversing ( Traversing )
import Rel8.Expr
import Rel8.Schema


newtype Query a b =
  Query (Star (State ()) a b)
  deriving (Functor, Applicative, Category, Profunctor, Strong, Choice, Traversing)
  deriving (Arrow, ArrowChoice) via Kleisli (State())


each :: Schema a -> Query x (Expr a)
each = undefined


optional :: Query a (Expr b) -> Query a (Expr (Maybe b))
optional = undefined


where_ :: Query Bool ()
where_ = undefined


catMaybe_ :: Query a (Expr (Maybe b)) -> Query a (Expr b)
catMaybe_ = undefined
