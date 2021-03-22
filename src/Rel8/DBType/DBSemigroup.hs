module Rel8.DBType.DBSemigroup ( DBSemigroup(..) ) where

-- rel8
import Rel8.DBType ( DBType )
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( liftOpNull )
import Rel8.PrimitiveType ( PrimitiveType )


class DBType a => DBSemigroup a where
  (<>.) :: Expr a -> Expr a -> Expr a


instance (PrimitiveType a, DBSemigroup a) => DBSemigroup (Maybe a) where
  (<>.) = liftOpNull (<>.)
