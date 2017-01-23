{-# LANGUAGE RankNTypes #-}

module Rel8.Internal.Generic where

import Rel8.Internal.Expr
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

--------------------------------------------------------------------------------
-- | The class of values that can be traversed for 'O.PrimExpr's.

class MapPrimExpr s where
  mapPrimExpr :: Applicative f => (O.PrimExpr -> f O.PrimExpr) -> s -> f s

instance MapPrimExpr (Expr column) where
  mapPrimExpr f (Expr a) = fmap Expr (f a)
