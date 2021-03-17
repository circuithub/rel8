{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.DBOrd
  ( DBOrd(..)
  )
where

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Prelude

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- rel8
import Rel8.Type.DBEq ( DBEq )
import Rel8.Expr ( Expr )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( DiffTime, NominalDiffTime, UTCTime )
import Data.Time.LocalTime ( TimeOfDay, LocalTime )

-- uuid
import Data.UUID ( UUID )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Expr.Opaleye ( zipPrimExprsWith )


type DBOrd :: Type -> Constraint
class DBEq a => DBOrd a where
  (>.) :: Expr a -> Expr a -> Expr Bool
  (>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:>))

  (<.) :: Expr a -> Expr a -> Expr Bool
  (<.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:<))

  (>=.) :: Expr a -> Expr a -> Expr Bool
  (>=.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:>=))

  (<=.) :: Expr a -> Expr a -> Expr Bool
  (<=.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:<=))


instance DBOrd Bool
instance DBOrd Char
instance DBOrd Int16
instance DBOrd Int32
instance DBOrd Int64
instance DBOrd Float
instance DBOrd Double
instance DBOrd Scientific
instance DBOrd UTCTime
instance DBOrd Day
instance DBOrd LocalTime
instance DBOrd TimeOfDay
instance DBOrd DiffTime
instance DBOrd NominalDiffTime
instance DBOrd Text
instance DBOrd Lazy.Text
instance DBOrd (CI Text)
instance DBOrd (CI Lazy.Text)
instance DBOrd ByteString
instance DBOrd Lazy.ByteString
instance DBOrd UUID
