module Rel8.DBType.DBNum ( DBNum(..), DBFractional(..) ) where


-- base
import Data.Int ( Int16, Int32, Int64 )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DBType ( DBType )
import Rel8.Expr ( Expr )
import Rel8.Expr.Function ( function )
import Rel8.Expr.Opaleye ( binExpr, fromPrimExpr, mapPrimExpr, recast )

-- scientific
import Data.Scientific ( Scientific )


class DBType a => DBNum a where
  (+.) :: Expr a -> Expr a -> Expr a
  (-.) :: Expr a -> Expr a -> Expr a
  (*.) :: Expr a -> Expr a -> Expr a
  absExpr :: Expr a -> Expr a
  signumExpr :: Expr a -> Expr a
  fromIntegerExpr :: Integer -> Expr a
  negateExpr :: Expr a -> Expr a

  (+.) = binExpr (Opaleye.:+)
  (-.) = binExpr (Opaleye.:-)
  (*.) = binExpr (Opaleye.:*)
  absExpr = function "abs"
  signumExpr = mapPrimExpr (Opaleye.UnExpr (Opaleye.UnOpOther "SIGN"))
  negateExpr = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNegate)
  fromIntegerExpr = recast . fromPrimExpr . Opaleye.ConstExpr . Opaleye.IntegerLit


class DBNum a => DBFractional a where
  (/.) :: Expr a -> Expr a -> Expr a
  fromRationalExpr :: Rational -> Expr a

  (/.) = binExpr (Opaleye.:/)
  fromRationalExpr = recast . fromPrimExpr . Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac


instance DBNum Int16


instance DBNum Int32


instance DBNum Int64


instance DBNum Float


instance DBNum Double


instance DBNum Scientific


instance DBFractional Float


instance DBFractional Double


instance DBFractional Scientific
