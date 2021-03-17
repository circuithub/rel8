{-# language StandaloneKindSignatures #-}

module Rel8.Type.DBMonoid ( DBMonoid(..) ) where


import Data.Kind ( Constraint, Type )
import {-# source #-} Rel8.Expr ( Expr )
import Rel8.Type.DBSemigroup ( DBSemigroup )


type DBMonoid :: Type -> Constraint
class DBSemigroup a => DBMonoid a where
  memptyExpr :: Expr a
