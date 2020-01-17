{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Expr ( Expr ) where

import Data.Functor.Identity
import Data.Kind
import Rel8.FromRow
import Rel8.Table
import {-# source #-} Rel8.Query


-- | Typed SQL expressions
data Expr ( m :: Type -> Type ) a


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent parts.
instance ( expr ~ Expr Query, identity ~ Identity ) => FromRow ( t expr ) ( t identity )


instance m ~ Query => FromRow ( Expr m Int ) Int


instance Table ( Expr m a ) where
  type ExprIn ( Expr m a ) = Expr m


(==.) :: Expr m a -> Expr m a -> expr Bool
(==.) = undefined
