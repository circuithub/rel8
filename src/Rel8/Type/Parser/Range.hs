{-# language OverloadedStrings #-}

module Rel8.Type.Parser.Range
  ( range
  , multirange
  , int4range
  , int8range
  , numrange
  , tsrange
  , tstzrange
  , daterange
  , int4multirange
  , int8multirange
  , nummultirange
  , tsmultirange
  , tstzmultirange
  , datemultirange
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>), optional)
import Data.Functor (void)
import Data.Int (Int32, Int64)
import Prelude

-- postgresql-binary
import qualified PostgreSQL.Binary.Range as Range

-- scientific
import Data.Scientific (Scientific)

-- rel8
import qualified Rel8.Type.Parser.Time as Parser

-- time
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)


range :: A.Parser a -> A.Parser (Range.Range a)
range boundsParser = empty <|> range'
  where
    range' = do
      open <- openMarker
      lower <- optional boundsParser
      void $ A.char ','
      upper <- optional boundsParser
      close <- closeMarker
      pure $ Range.Range (maybe Range.Inf open lower) (maybe Range.Inf close upper)
    empty = A.string "empty" *> pure Range.Empty
    openMarker = (A.char '[' *> pure Range.Incl) <|> (A.char '(' *> pure Range.Excl)
    closeMarker = (A.char ']' *> pure Range.Incl) <|> (A.char ')' *> pure Range.Excl)


multirange :: A.Parser a -> A.Parser (Range.Multirange a)
multirange boundsParser =
  A.char '{' *> A.sepBy element (A.char delimiter) <* A.char '}'
  where
    delimiter = ','
    element = range boundsParser


int4range :: A.Parser (Range.Range Int32)
int4range = range $ A.signed A.decimal


int8range :: A.Parser (Range.Range Int64)
int8range = range $ A.signed A.decimal


numrange :: A.Parser (Range.Range Scientific)
numrange = range A.scientific


tsrange :: A.Parser (Range.Range LocalTime)
tsrange = range Parser.localTime


tstzrange :: A.Parser (Range.Range UTCTime)
tstzrange = range Parser.utcTime


daterange :: A.Parser (Range.Range Day)
daterange = range Parser.day


int4multirange :: A.Parser (Range.Multirange Int32)
int4multirange = multirange $ A.signed A.decimal


int8multirange :: A.Parser (Range.Multirange Int64)
int8multirange = multirange $ A.signed A.decimal


nummultirange :: A.Parser (Range.Multirange Scientific)
nummultirange = multirange A.scientific


tsmultirange :: A.Parser (Range.Multirange LocalTime)
tsmultirange = multirange Parser.localTime


tstzmultirange :: A.Parser (Range.Multirange UTCTime)
tstzmultirange = multirange Parser.utcTime


datemultirange :: A.Parser (Range.Multirange Day)
datemultirange = multirange Parser.day
