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

  -- CalendarDiffTime
  , second
  , seconds
  , minute
  , minutes
  , hour
  , hours
  , day
  , days
  , month
  , months
  , year
  , years
  , week
  , weeks
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
import Data.Time ( CalendarDiffTime, Day, UTCTime )


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


addTime :: Expr CalendarDiffTime -> Expr UTCTime -> Expr UTCTime
addTime = flip (dbBinOp "+")


diffTime :: Expr UTCTime -> Expr UTCTime -> Expr CalendarDiffTime
diffTime = dbBinOp "-"


subtractTime :: Expr CalendarDiffTime -> Expr UTCTime -> Expr UTCTime
subtractTime = flip (dbBinOp "-")


scaleInterval :: Expr Double -> Expr CalendarDiffTime -> Expr CalendarDiffTime
scaleInterval = dbBinOp "*"


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
singleton unit = unsafeCastExpr "interval" $ unsafeLiteral $ "'1 " ++ unit ++ "'"
