{-# language BangPatterns #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

{-# options_ghc -Wno-partial-type-signatures #-}
-- bytestring does not export Monoidal so we can't write a complete type
-- signature for 'divide'

{-# options_ghc -Wno-unused-top-binds #-}
-- GHC considers the YMD pattern unused but we use its selectors

module Rel8.Type.Builder.Time (
  calendarDiffTime,
  day,
  localTime,
  timeOfDay,
  utcTime,
) where

-- base
import Data.Char (chr)
import Data.Fixed (Fixed (MkFixed), Pico)
import Data.Int (Int32, Int64)
import Prelude hiding ((<>))

-- bytestring
import Data.ByteString.Builder (Builder, string7)
import Data.ByteString.Builder.Prim (
  BoundedPrim, condB, emptyB, liftFixedToBounded,
  FixedPrim, char8, int32Dec,
  (>$<), (>*<),
 )

-- time
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (UTCTime (utctDay, utctDayTime))
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (
  CalendarDiffTime,
  LocalTime (localDay, localTimeOfDay),
  TimeOfDay (todHour, todMin, todSec),
  timeToTimeOfDay
 )


digit :: FixedPrim Int
digit = (\x -> chr (x + 48)) >$< char8


digits2 :: FixedPrim Int
digits2 = divide (`quotRem` 10) digit digit


digits3 :: FixedPrim Int
digits3 = divide (`quotRem` 10) digits2 digit


digits4 :: FixedPrim Int
digits4 = divide (`quotRem` 10) digits3 digit


frac :: BoundedPrim Int64
frac = condB (== 0) emptyB $ liftFixedToBounded (char '.') <> trunc12
  where
    trunc12 =
      divide
        (`quotRem` 1_000_000)
        (fromIntegral >$< ifZero trunc6 (liftFixedToBounded digits6))
        (fromIntegral >$< nonZero trunc6)

    digitB = liftFixedToBounded digit

    digits6 = divide (`quotRem` 10) digits5 digit
    digits5 = divide (`quotRem` 10) digits4 digit

    trunc6 = divide (`quotRem` 100_000) digitB trunc5
    trunc5 = nonZero $ divide (`quotRem` 10_000) digitB trunc4
    trunc4 = nonZero $ divide (`quotRem` 1_000) digitB trunc3
    trunc3 = nonZero $ divide (`quotRem` 100) digitB trunc2
    trunc2 = nonZero $ divide (`quotRem` 10) digitB trunc1
    trunc1 = nonZero digitB

    nonZero = ifZero emptyB
    ifZero = condB (== 0)


seconds :: BoundedPrim Pico
seconds =
  (\(MkFixed s) -> fromIntegral s `quotRem` 1_000_000_000_000) >$<
  (liftFixedToBounded (fromIntegral >$< digits2) >*< frac)


year :: BoundedPrim Int32
year = condB (>= 10000) int32Dec (liftFixedToBounded (fromIntegral >$< digits4))


day :: BoundedPrim Day
day =
  (fromIntegral . ymdYear >$< year) <>
  liftFixedToBounded
    ( char '-' <> (ymdMonth >$< digits2) <> char '-' <> (ymdDay >$< digits2)
    )


pattern YMD :: Integer -> Int -> Int -> Day
pattern YMD {ymdYear, ymdMonth, ymdDay} <-
  (toGregorian -> (ymdYear, ymdMonth, ymdDay))


timeOfDay :: BoundedPrim TimeOfDay
timeOfDay =
  liftFixedToBounded
    ( (todHour >$< digits2) <> char ':' <> (todMin >$< digits2) <> char ':'
    ) <>
  (todSec >$< seconds)


utcTime :: BoundedPrim UTCTime
utcTime =
  (utctDay >$< day) <>
  liftFixedToBounded (char ' ') <>
  (timeToTimeOfDay . utctDayTime >$< timeOfDay) <>
  liftFixedToBounded (char 'Z')


localTime :: BoundedPrim LocalTime
localTime =
  (localDay >$< day) <>
  liftFixedToBounded (char ' ') <>
  (localTimeOfDay >$< timeOfDay)


calendarDiffTime :: CalendarDiffTime -> Builder
calendarDiffTime = string7 . iso8601Show


char :: Char -> FixedPrim a
char c = (\_ -> c) >$< char8


(<>) :: _ => f a -> f a -> f a
(<>) = divide (\a -> (a, a))
infixr 6 <>


divide :: _ => (a -> (b, c)) -> f b -> f c -> f a
divide f a b = f >$< (a >*< b)
