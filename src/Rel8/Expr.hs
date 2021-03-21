{-# LANGUAGE RoleAnnotations #-}
module Rel8.Expr ( Expr(..) ) where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- | Typed SQL expressions
type role Expr representational


newtype Expr a = Expr { toPrimExpr :: Opaleye.PrimExpr }
