{-# language KindSignatures #-}
{-# language TypeApplications #-}

module Rel8.DBType.DBOrd ( DBOrd(..) ) where

-- base
import Data.Int ( Int32, Int64 )
import Data.Kind ( Type )

-- opaleye
import qualified Opaleye.Operators as Opaleye
import qualified Opaleye.PGTypes as Opaleye

-- rel8
import Rel8.DBType.DBEq ( DBEq )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( columnToExpr, exprToColumn )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )

-- time
import Data.Time ( Day, UTCTime )


class DBEq a => DBOrd (a :: Type) where
  -- | The PostgreSQL @<@ operator.
  (<.) :: Expr a -> Expr a -> Expr Bool
  a <. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..< exprToColumn b)

  -- | The PostgreSQL @<=@ operator.
  (<=.) :: Expr a -> Expr a -> Expr Bool
  a <=. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..<= exprToColumn b)

  -- | The PostgreSQL @>@ operator.
  (>.) :: Expr a -> Expr a -> Expr Bool
  a >. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..> exprToColumn b)

  -- | The PostgreSQL @>@ operator.
  (>=.) :: Expr a -> Expr a -> Expr Bool
  a >=. b = columnToExpr (exprToColumn @_ @Opaleye.PGInt8 a Opaleye..>= exprToColumn b)


instance DBOrd Bool where


instance DBOrd Int32 where


instance DBOrd Int64 where


instance DBOrd Text where


instance DBOrd UTCTime where


instance DBOrd Day where


instance DBOrd Double where


instance DBOrd Scientific where
