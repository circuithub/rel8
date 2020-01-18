{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

module Rel8.Table where

import Rel8.Column


-- | The class of Haskell types that represent SQL tables.
class Table t where
  -- | Calculate functor of expressions that match the expression functor for
  -- a given type. For examle, if we have a higher-kinded data type @t@
  -- parameterised by @Expr m@, @ExprIn (t (Expr m)) = Expr m@.
  type ExprIn t :: * -> *


instance Table (C f a) where
  type ExprIn (C f a) = f


instance Table ( t ( expr :: * -> * ) ) where
  type ExprIn ( t expr ) =
    expr
