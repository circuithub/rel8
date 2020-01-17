{-# language FlexibleInstances #-}

module Rel8.DBEq where

import Rel8.Expr


-- | The class of database types that support equality.
class DBTypeEq a where
  eqExprs :: Expr m a -> Expr m a -> Expr m Bool


instance DBTypeEq String where
  eqExprs = undefined


instance DBTypeEq Int where
  eqExprs = undefined
