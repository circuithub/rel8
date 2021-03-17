{-# language KindSignatures #-}
{-# language StandaloneKindSignatures #-}

{-# LANGUAGE FlexibleInstances #-}
module Rel8.Type.DBEq ( DBEq(..) ) where


import Data.Kind ( Constraint, Type )
import Rel8.Expr ( Expr )
import Rel8.Type ( DBType )
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Time (UTCTime, Day, LocalTime, TimeOfDay, DiffTime, NominalDiffTime)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.ByteString.Lazy as Lazy
import Data.CaseInsensitive (CI)
import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Data.Aeson (Value)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Expr.Opaleye ( zipPrimExprsWith )


type DBEq :: Type -> Constraint
class DBType a => DBEq a where
  (==.) :: Expr a -> Expr a -> Expr Bool
  (==.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:==))


instance DBEq Bool
instance DBEq Char
instance DBEq Int16
instance DBEq Int32
instance DBEq Int64
instance DBEq Float
instance DBEq Double
instance DBEq Scientific
instance DBEq UTCTime
instance DBEq Day
instance DBEq LocalTime
instance DBEq TimeOfDay
instance DBEq DiffTime
instance DBEq NominalDiffTime
instance DBEq Text
instance DBEq Lazy.Text
instance DBEq (CI Text)
instance DBEq (CI Lazy.Text)
instance DBEq ByteString
instance DBEq Lazy.ByteString
instance DBEq UUID
instance DBEq Value

