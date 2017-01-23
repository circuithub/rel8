{-# LANGUAGE RoleAnnotations #-}

module Rel8.Internal.Aggregate where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

-- | Used to tag 'Expr's that are the result of aggregation
data Aggregate a = Aggregate (Maybe O.AggrOp) O.PrimExpr O.AggrDistinct

type role Aggregate representational
