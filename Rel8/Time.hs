module Rel8.Time
  (
  -- Day
    today
  , toDay
  , fromDay
  , addDays
  , diffDays
  , subtractDays

  -- UTCTime
  , now
  , addTime
  , diffTime
  , subtractTime

  -- NominalDiffTime
  , seconds
  , minutes
  , hours
  , days
  , months
  , years
  ) where

-- base
import Data.Int ( Int32 )

-- rel8
import Rel8
  ( Expr
  , dbBinOp
  , dbFunction
  , dbNow
  , unsafeCastExpr
  , unsafeLiteral
  )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( UTCTime, NominalDiffTime )


today :: Expr Day
today = toDay now


toDay :: Expr UTCTime -> Expr Day
toDay = dbFunction "date"


fromDay :: Expr Day -> Expr UTCTime
fromDay = unsafeCastExpr "timestamptz"


addDays :: Expr Int32 -> Expr Day -> Expr Day
addDays = flip (dbBinOp "+")


diffDays :: Expr Day -> Expr Day -> Expr Int32
diffDays = dbBinOp "-"


subtractDays :: Expr Int32 -> Expr Day -> Expr Day
subtractDays = flip (dbBinOp "-")


now :: Expr UTCTime
now = dbNow


addTime :: Expr NominalDiffTime -> Expr UTCTime -> Expr UTCTime
addTime = flip (dbBinOp "+")


diffTime :: Expr UTCTime -> Expr UTCTime -> Expr NominalDiffTime
diffTime = dbBinOp "-"


subtractTime :: Expr NominalDiffTime -> Expr UTCTime -> Expr UTCTime
subtractTime = flip (dbBinOp "-")


seconds :: Int -> Expr NominalDiffTime
seconds = interval "seconds"


minutes :: Int -> Expr NominalDiffTime
minutes = interval "minutes"


hours ::  Int -> Expr NominalDiffTime
hours = interval "hours"


days ::  Int -> Expr NominalDiffTime
days = interval "days"


months ::  Int -> Expr NominalDiffTime
months = interval "months"


years ::  Int -> Expr NominalDiffTime
years = interval "years"


interval :: String -> Int -> Expr NominalDiffTime
interval unit quantity =
  unsafeLiteral ("interval '" ++ show quantity ++ " " ++ unit ++ "'")
