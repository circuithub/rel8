{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Rel8.Type.Parser.Time
  ( calendarDiffTime
  , day
  , localTime
  , timeOfDay
  , utcTime
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>), optional)
import Data.Bits ((.&.))
import Data.Bool (bool)
import Data.Fixed (Fixed (MkFixed), Pico, divMod')
import Data.Functor (void)
import Data.Int (Int64)
import Prelude

-- bytestring
import qualified Data.ByteString as BS

-- time
import Data.Time.Calendar (Day, addDays, fromGregorianValid)
import Data.Time.Clock (DiffTime, UTCTime (UTCTime))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime
  ( CalendarDiffTime (CalendarDiffTime)
  , LocalTime (LocalTime)
  , TimeOfDay (TimeOfDay)
  , sinceMidnight
  )

-- utf8
import qualified Data.ByteString.UTF8 as UTF8


day :: A.Parser Day
day = do
  y <- A.decimal <* A.char '-'
  m <- twoDigits <* A.char '-'
  d <- twoDigits
  maybe (fail "Day: invalid date") pure $ fromGregorianValid y m d


timeOfDay :: A.Parser TimeOfDay
timeOfDay = do
  h <- twoDigits
  m <- A.char ':' *> twoDigits
  s <- A.char ':' *> secondsParser
  if h < 24 && m < 60 && s <= 60
    then pure $ TimeOfDay h m s
    else fail "TimeOfDay: invalid time"


localTime :: A.Parser LocalTime
localTime = LocalTime <$> day <* separator <*> timeOfDay
  where
    separator = A.char ' ' <|> A.char 'T'


utcTime :: A.Parser UTCTime
utcTime = do
  LocalTime date time <- localTime
  tz <- timeZone
  let
    (days, time') = (sinceMidnight time + tz) `divMod'` oneDay
      where
        oneDay = 24 * 60 * 60
    date' = addDays days date
  pure $ UTCTime date' time'


calendarDiffTime :: A.Parser CalendarDiffTime
calendarDiffTime = iso8601 <|> postgres
  where
    iso8601 = A.takeByteString >>= iso8601ParseM . UTF8.toString
    at = optional (A.char '@') *> A.skipSpace
    plural unit = A.skipSpace <* (unit <* optional "s") <* A.skipSpace
    parseMonths = sql <|> postgresql
      where
        sql = A.signed $ do
          years <- A.decimal <* A.char '-'
          months <- A.decimal <* A.skipSpace
          pure $ years * 12 + months
        postgresql = do
          at
          years <- A.signed A.decimal <* plural "year" <|> pure 0
          months <- A.signed A.decimal <* plural "mon" <|> pure 0
          pure $ years * 12 + months
    parseTime = (+) <$> parseDays <*> time
      where
        time = realToFrac <$> (sql <|> postgresql)
          where
            sql = A.signed $ do
              h <- A.signed A.decimal <* A.char ':'
              m <- twoDigits <* A.char ':'
              s <- secondsParser
              pure $ fromIntegral (((h * 60) + m) * 60) + s
            postgresql = do
              h <- A.signed A.decimal <* plural "hour" <|> pure 0
              m <- A.signed A.decimal <* plural "min" <|> pure 0
              s <- secondsParser <* plural "sec" <|> pure 0
              pure $ fromIntegral @Int (((h * 60) + m) * 60) + s
        parseDays = do
          days <- A.signed A.decimal <* (plural "days" <|> skipSpace1) <|> pure 0
          pure $ fromIntegral @Int days * 24 * 60 * 60
    postgres = do
      months <- parseMonths
      time <- parseTime
      ago <- (True <$ (A.skipSpace *> "ago")) <|> pure False
      pure $ CalendarDiffTime (bool id negate ago months) (bool id negate ago time)


secondsParser :: A.Parser Pico
secondsParser = do
  integral <- twoDigits
  mfractional <- optional (A.char '.' *> A.takeWhile1 A.isDigit)
  pure $ case mfractional of
    Nothing -> fromIntegral integral
    Just fractional -> parseFraction (fromIntegral integral) fractional
 where
  parseFraction integral digits = MkFixed (fromIntegral (n * 10 ^ e))
    where
      e = max 0 (12 - BS.length digits)
      n = BS.foldl' go (integral :: Int64) (BS.take 12 digits)
        where
          go acc digit = 10 * acc + fromIntegral (fromEnum digit .&. 0xf)


twoDigits :: A.Parser Int
twoDigits = do
  u <- A.digit
  l <- A.digit
  pure $ fromEnum u .&. 0xf * 10 + fromEnum l .&. 0xf


timeZone :: A.Parser DiffTime
timeZone = 0 <$ A.char 'Z' <|> diffTime


diffTime :: A.Parser DiffTime
diffTime = A.signed $ do
  h <- twoDigits
  m <- A.char ':' *> twoDigits <|> pure 0
  s <- A.char ':' *> secondsParser <|> pure 0
  pure $ sinceMidnight $ TimeOfDay h m s


skipSpace1 :: A.Parser ()
skipSpace1 = void $ A.takeWhile1 A.isSpace
