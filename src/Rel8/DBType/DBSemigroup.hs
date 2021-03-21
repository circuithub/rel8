module Rel8.DBType.DBSemigroup ( DBSemigroup(..) ) where

import Rel8.Info ( HasInfo )
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( liftOpNull )
import Rel8.DBType (DBType)


class HasInfo a => DBSemigroup a where
  (<>.) :: Expr a -> Expr a -> Expr a


instance (DBType a, DBSemigroup a) => DBSemigroup (Maybe a) where
  (<>.) = liftOpNull (<>.)
