{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

module Rel8.Table where

import Rel8.Column


class Table t where
  type ExprIn t :: * -> *


instance Table (C f a) where
  type ExprIn (C f a) = f


instance Table ( t ( expr :: * -> * ) ) where
  type ExprIn ( t expr ) =
    expr
