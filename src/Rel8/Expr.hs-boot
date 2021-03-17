{-# LANGUAGE RoleAnnotations #-}
module Rel8.Expr ( Expr(..) ) where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

type role Expr representational
newtype Expr a = Expr { toPrimExpr :: Opaleye.PrimExpr }
