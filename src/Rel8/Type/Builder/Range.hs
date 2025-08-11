{-# language OverloadedStrings #-}

module Rel8.Type.Builder.Range (
  range,
  multirange,
  int4range,
  int8range,
  numrange,
  tsrange,
  tstzrange,
  daterange,
  int4multirange,
  int8multirange,
  nummultirange,
  tsmultirange,
  tstzmultirange,
  datemultirange,
) where

-- base
import Data.Int (Int32, Int64)
import Prelude

-- bytestring
import Data.ByteString.Builder (Builder, string7, char8, int32Dec, int64Dec)
import Data.ByteString.Builder.Prim (primBounded)

-- rel8
import qualified Rel8.Type.Builder.Fold as Fold
import qualified Rel8.Type.Builder.Time as Builder

-- postgresql-binary
import qualified PostgreSQL.Binary.Range as Range

-- scientific
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.Scientific (Scientific)

-- time
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)


range :: (a -> Builder) -> Range.Range a -> Builder
range _ Range.Empty = string7 "empty"
range boundsBuilder (Range.Range lower upper) =
  left <> char8 ',' <> right
  where
    left =
      case lower of
        Range.Inf -> char8 '('
        Range.Incl a -> char8 '[' <> boundsBuilder a <> char8 ']'
        Range.Excl a -> char8 '(' <> boundsBuilder a <> char8 ')'
    right =
      case upper of
        Range.Inf -> char8 ')'
        Range.Incl a -> boundsBuilder a <> char8 ']'
        Range.Excl a -> boundsBuilder a <> char8 ')'


multirange :: (a -> Builder) -> Range.Multirange a -> Builder
multirange boundsBuilder ranges =
  char8 '{' <> Fold.interfoldMap (char8 ',') (range boundsBuilder) ranges <> char8 '}'


int4range :: Range.Range Int32 -> Builder
int4range = range int32Dec


int8range :: Range.Range Int64 -> Builder
int8range = range int64Dec


numrange :: Range.Range Scientific -> Builder
numrange = range scientificBuilder


tsrange :: Range.Range LocalTime -> Builder
tsrange = range (primBounded Builder.localTime)


tstzrange :: Range.Range UTCTime -> Builder
tstzrange = range (primBounded Builder.utcTime)


daterange :: Range.Range Day -> Builder
daterange = range (primBounded Builder.day)


int4multirange :: Range.Multirange Int32 -> Builder
int4multirange = multirange int32Dec


int8multirange :: Range.Multirange Int64 -> Builder
int8multirange = multirange int64Dec


nummultirange :: Range.Multirange Scientific -> Builder
nummultirange = multirange scientificBuilder


tsmultirange :: Range.Multirange LocalTime -> Builder
tsmultirange = multirange (primBounded Builder.localTime)


tstzmultirange :: Range.Multirange UTCTime -> Builder
tstzmultirange = multirange (primBounded Builder.utcTime)


datemultirange :: Range.Multirange Day -> Builder
datemultirange = multirange (primBounded Builder.day)
