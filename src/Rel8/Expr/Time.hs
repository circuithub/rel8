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

  -- * Working with @CalendarDiffTime@
  , scaleInterval
  , second, seconds
  , minute, minutes
  , hour, hours
  , day, days
  , week, weeks
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
import Data.Time.Clock ( UTCTime )
import Data.Time.LocalTime ( CalendarDiffTime )


-- | Corresponds to @date(now())@.
today :: Expr Day
today = toDay now


-- | Corresponds to calling the @date@ function with a given time.
toDay :: Expr UTCTime -> Expr Day
toDay = unsafeCastExpr


-- | Corresponds to @x::timestamptz@.
fromDay :: Expr Day -> Expr UTCTime
fromDay = unsafeCastExpr


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
addTime :: Expr CalendarDiffTime -> Expr UTCTime -> Expr UTCTime
addTime = flip (binaryOperator "+")


-- | Find the duration between two times.
diffTime :: Expr UTCTime -> Expr UTCTime -> Expr CalendarDiffTime
diffTime = binaryOperator "-"


-- | Subtract a time interval from a point in time, yielding a new point in time.
subtractTime :: Expr CalendarDiffTime -> Expr UTCTime -> Expr UTCTime
subtractTime = flip (binaryOperator "-")


scaleInterval :: Expr Double -> Expr CalendarDiffTime -> Expr CalendarDiffTime
scaleInterval = binaryOperator "*"


-- | An interval of one second.
second :: Expr CalendarDiffTime
second = singleton "second"


-- | Create a literal interval from a number of seconds.
seconds :: Expr Double -> Expr CalendarDiffTime
seconds = (`scaleInterval` second)


-- | An interval of one minute.
minute :: Expr CalendarDiffTime
minute = singleton "minute"


-- | Create a literal interval from a number of minutes.
minutes :: Expr Double -> Expr CalendarDiffTime
minutes = (`scaleInterval` minute)


-- | An interval of one hour.
hour :: Expr CalendarDiffTime
hour = singleton "hour"


-- | Create a literal interval from a number of hours.
hours :: Expr Double -> Expr CalendarDiffTime
hours = (`scaleInterval` hour)


-- | An interval of one day.
day :: Expr CalendarDiffTime
day = singleton "day"


-- | Create a literal interval from a number of days.
days ::  Expr Double -> Expr CalendarDiffTime
days = (`scaleInterval` day)


-- | An interval of one week.
week :: Expr CalendarDiffTime
week = singleton "week"


-- | Create a literal interval from a number of weeks.
weeks ::  Expr Double -> Expr CalendarDiffTime
weeks = (`scaleInterval` week)


-- | An interval of one month.
month :: Expr CalendarDiffTime
month = singleton "month"


-- | Create a literal interval from a number of months.
months ::  Expr Double -> Expr CalendarDiffTime
months = (`scaleInterval` month)


-- | An interval of one year.
year :: Expr CalendarDiffTime
year = singleton "year"


-- | Create a literal interval from a number of years.
years ::  Expr Double -> Expr CalendarDiffTime
years = (`scaleInterval` year)


singleton :: String -> Expr CalendarDiffTime
singleton unit = unsafeLiteral $ "'1 " ++ unit ++ "'"
