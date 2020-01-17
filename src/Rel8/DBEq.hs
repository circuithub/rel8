{-# language FlexibleInstances #-}

module Rel8.DBEq where

import Rel8.Expr
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


-- | The class of database types that can be compared for equality in queries.
class DBEq a where
  eqExprs :: Expr m a -> Expr m a -> Expr m Bool
  eqExprs ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr (Opaleye.:==) a b )


instance DBEq String


instance DBEq Int
