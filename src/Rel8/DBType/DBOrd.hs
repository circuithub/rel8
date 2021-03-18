{-# language KindSignatures #-}

module Rel8.DBType.DBOrd ( DBOrd(..) ) where

-- base
import Data.Int ( Int32, Int64 )
import Data.Kind ( Type )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DBType.DBEq ( DBEq )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( binExpr )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )

-- time
import Data.Time ( Day, UTCTime )


class DBEq a => DBOrd (a :: Type) where
  -- | The PostgreSQL @<@ operator.
  (<.) :: Expr a -> Expr a -> Expr Bool
  (<.) = binExpr (Opaleye.:<)

  -- | The PostgreSQL @<=@ operator.
  (<=.) :: Expr a -> Expr a -> Expr Bool
  (<=.) = binExpr (Opaleye.:<=)

  -- | The PostgreSQL @>@ operator.
  (>.) :: Expr a -> Expr a -> Expr Bool
  (>.) = binExpr (Opaleye.:>)

  -- | The PostgreSQL @>@ operator.
  (>=.) :: Expr a -> Expr a -> Expr Bool
  (>=.) = binExpr (Opaleye.:>=)


instance DBOrd Bool where


instance DBOrd Int32 where


instance DBOrd Int64 where


instance DBOrd Text where


instance DBOrd UTCTime where


instance DBOrd Day where


instance DBOrd Double where


instance DBOrd Scientific where
