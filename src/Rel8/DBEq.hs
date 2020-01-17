{-# language FlexibleInstances #-}

module Rel8.DBEq where

import Control.Applicative
import Data.Proxy
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.Table


-- | The class of database types that support equality.
class DBTypeEq a where
  eqExprs :: Expr m a -> Expr m a -> Expr m Bool


instance DBTypeEq String where
  eqExprs = undefined


instance DBTypeEq Int where
  eqExprs = undefined
