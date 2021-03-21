module Rel8.DBType.DBMonoid ( DBMonoid(..) ) where

-- rel8
import Rel8.DBType ( DBType )
import Rel8.DBType.DBSemigroup ( DBSemigroup )
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( liftNull )


class DBSemigroup a => DBMonoid a where
  memptyExpr :: Expr a


instance (DBType a, DBMonoid a) => DBMonoid (Maybe a) where
  memptyExpr = liftNull memptyExpr
