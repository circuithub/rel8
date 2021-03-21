module Rel8.DBType.DBMonoid ( DBMonoid(..) ) where

import Rel8.DBType.DBSemigroup ( DBSemigroup )
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( liftNull )
import Rel8.DBType (DBType)


class DBSemigroup a => DBMonoid a where
  memptyExpr :: Expr a


instance (DBType a, DBMonoid a) => DBMonoid (Maybe a) where
  memptyExpr = liftNull memptyExpr
