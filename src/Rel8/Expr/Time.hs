module Rel8.Expr.Time
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
  , scaleInterval
  , second, seconds
  , minute, minutes
  , hour, hours
  , day, days
  , month, months
  , year, years
  ) where

-- base
import Data.Int ( Int32 )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Function ( binaryOperator, nullaryFunction )
import Rel8.Expr.Opaleye ( unsafeCastExpr, unsafeLiteral )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( UTCTime, NominalDiffTime )


-- | Corresponds to @date(now())@.
today :: Expr nullability Day
today = toDay now


-- | Corresponds to calling the @date@ function with a given time.
toDay :: Expr nullability UTCTime -> Expr nullability Day
toDay = unsafeCastExpr


-- | Corresponds to @x::timestamptz@.
fromDay :: Expr nullability Day -> Expr nullability UTCTime
fromDay = unsafeCastExpr


-- | Move forward a given number of days from a particular day.
addDays :: ()
  => Expr nullability Int32 -> Expr nullability Day -> Expr nullability Day
addDays = flip (binaryOperator "+")


-- | Find the number of days between two days. Corresponds to the @-@ operator.
diffDays :: ()
  => Expr nullability Day -> Expr nullability Day -> Expr nullability Int32
diffDays = binaryOperator "-"


-- | Subtract a given number of days from a particular 'Day'. 
subtractDays :: ()
  => Expr nullability Int32
  -> Expr nullability Day
  -> Expr nullability Day
subtractDays = flip (binaryOperator "-")


-- | Corresponds to @now()@.
now :: Expr nullability UTCTime
now = nullaryFunction "now"


-- | Add a time interval to a point in time, yielding a new point in time.
addTime :: ()
  => Expr nullability NominalDiffTime
  -> Expr nullability UTCTime
  -> Expr nullability UTCTime
addTime = flip (binaryOperator "+")


-- | Find the duration between two times.
diffTime :: ()
  => Expr nullability UTCTime
  -> Expr nullability UTCTime
  -> Expr nullability NominalDiffTime
diffTime = binaryOperator "-"


-- | Subtract a time interval from a point in time, yielding a new point in time.
subtractTime :: ()
  => Expr nullability NominalDiffTime
  -> Expr nullability UTCTime
  -> Expr nullability UTCTime
subtractTime = flip (binaryOperator "-")


scaleInterval :: ()
  => Expr nullability Double
  -> Expr nullability NominalDiffTime
  -> Expr nullability NominalDiffTime
scaleInterval = binaryOperator "*"


-- | An interval of one second.
second :: Expr nullability NominalDiffTime
second = singleton "second"


-- | Create a literal interval from an integral number of seconds.
seconds :: Expr nullability Double -> Expr nullability NominalDiffTime
seconds = (`scaleInterval` second)


-- | An interval of one minute.
minute :: Expr nullability NominalDiffTime
minute = singleton "minute"


-- | Create a literal interval from an integral number of minutes.
minutes :: Expr nullability Double -> Expr nullability NominalDiffTime
minutes = (`scaleInterval` minute)


-- | An interval of one hour.
hour :: Expr nullability NominalDiffTime
hour = singleton "hour"


-- | Create a literal interval from an integral number of hours.
hours :: Expr nullability Double -> Expr nullability NominalDiffTime
hours = (`scaleInterval` hour)


-- | An interval of one day.
day :: Expr nullability NominalDiffTime
day = singleton "day"


-- | Create a literal interval from a number of days.
days ::  Expr nullability Double -> Expr nullability NominalDiffTime
days = (`scaleInterval` day)


-- | An interval of one month.
month :: Expr nullability NominalDiffTime
month = singleton "month"


-- | Create a literal interval from a number of months.
months ::  Expr nullability Double -> Expr nullability NominalDiffTime
months = (`scaleInterval` month)


-- | An interval of one year.
year :: Expr nullability NominalDiffTime
year = singleton "year"


-- | Create a literal interval from a number of years.
years ::  Expr nullability Double -> Expr nullability NominalDiffTime
years = (`scaleInterval` year)


singleton :: String -> Expr nullability NominalDiffTime
singleton unit = unsafeLiteral $ "'1 " ++ unit ++ "'"
