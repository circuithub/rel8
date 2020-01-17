{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RoleAnnotations #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Expr where

import Data.Kind
import Rel8.Table
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


-- | Typed SQL expressions
data Expr ( m :: Type -> Type ) a =
  Expr { toPrimExpr :: Opaleye.PrimExpr }

type role Expr representational nominal


instance Table ( Expr m a ) where
  type ExprIn ( Expr m a ) = Expr m
