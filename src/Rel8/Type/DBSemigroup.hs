{-# language KindSignatures #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.DBSemigroup ( DBSemigroup(..) ) where


import Data.Kind ( Constraint, Type )
import {-# source #-} Rel8.Expr ( Expr )
import Rel8.Type ( DBType )


type DBSemigroup :: Type -> Constraint
class DBType a => DBSemigroup a where
  (<>.) :: Expr a -> Expr a -> Expr a
