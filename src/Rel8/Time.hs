module Rel8.Time
  ( -- * Working with @Day@
    today
  , toDay
  , fromDay
  , addDays
  , diffDays
  , subtractDays

    -- * Working with @UTCTime@
  , now
  , addTime
  , diffTime
  , subtractTime

  -- Working with @NominalDiffTime@
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
  , binaryOperator
  , function
  , unsafeCastExpr
  , unsafeLiteral, nullaryFunction
  )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( UTCTime, NominalDiffTime )


-- | Corresponds to @date(now())@.
today :: Expr Day
today = toDay now


-- | Corresponds to calling the @date@ function with a given time.
toDay :: Expr UTCTime -> Expr Day
toDay = function "date"


-- | Corresponds to @x::timestamptz@.
fromDay :: Expr Day -> Expr UTCTime
fromDay = unsafeCastExpr "timestamptz"


-- | Move forward a given number of days from a particular day.
addDays :: Expr Int32 -> Expr Day -> Expr Day
addDays = flip (binaryOperator "+")


-- | Find the number of days between two days. Corresponds to the @-@ operator.
diffDays :: Expr Day -> Expr Day -> Expr Int32
diffDays = binaryOperator "-"


-- | Subtract a given number of days from a particular 'Day'. 
subtractDays :: Expr Int32 -> Expr Day -> Expr Day
subtractDays = flip (binaryOperator "-")


-- | Corresponds to @now()@.
now :: Expr UTCTime
now = nullaryFunction "now"


-- | Add a time interval to a point in time, yielding a new point in time.
addTime :: Expr NominalDiffTime -> Expr UTCTime -> Expr UTCTime
addTime = flip (binaryOperator "+")


-- | Find the duration between two times.
diffTime :: Expr UTCTime -> Expr UTCTime -> Expr NominalDiffTime
diffTime = binaryOperator "-"


-- | Subtract a time interval from a point in time, yielding a new point in time.
subtractTime :: Expr NominalDiffTime -> Expr UTCTime -> Expr UTCTime
subtractTime = flip (binaryOperator "-")


-- | Create a literal interval from an integral number of seconds.
seconds :: Int -> Expr NominalDiffTime
seconds = interval "seconds"


-- | Create a literal interval from an integral number of minutes.
minutes :: Int -> Expr NominalDiffTime
minutes = interval "minutes"


-- | Create a literal interval from an integral number of hours.
hours ::  Int -> Expr NominalDiffTime
hours = interval "hours"


-- | Create a literal interval from an integral number of days.
days ::  Int -> Expr NominalDiffTime
days = interval "days"


-- | Create a literal interval from an integral number of months.
months ::  Int -> Expr NominalDiffTime
months = interval "months"


-- | Create a literal interval from an integral number of years.
years ::  Int -> Expr NominalDiffTime
years = interval "years"


interval :: String -> Int -> Expr NominalDiffTime
interval unit quantity =
  unsafeLiteral ("interval '" ++ show quantity ++ " " ++ unit ++ "'")
