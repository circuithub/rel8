{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Rel8.Type.Range (
  DBRange (
    rangeTypeName, rangeDecoder, rangeEncoder,
    multirangeTypeName, multirangeDecoder, multirangeEncoder
  ),
) where

-- base
import Data.Int (Int32, Int64)

-- hasql
import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder

-- postgresql-binary
import qualified PostgreSQL.Binary.Range as PostgreSQL

-- rel8
import Rel8.Schema.QualifiedName (QualifiedName)
import Rel8.Type.Ord (DBOrd)

-- scientific
import Data.Scientific (Scientific)

-- time
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)


class DBOrd a => DBRange a where
  rangeTypeName :: QualifiedName
  rangeDecoder :: Decoder.Value (PostgreSQL.Range a)
  rangeEncoder :: Encoder.Value (PostgreSQL.Range a)

  multirangeTypeName :: QualifiedName
  multirangeDecoder :: Decoder.Value (PostgreSQL.Multirange a)
  multirangeEncoder :: Encoder.Value (PostgreSQL.Multirange a)


instance DBRange Int32 where
  rangeTypeName = "int4range"
  rangeDecoder = Decoder.int4range
  rangeEncoder = Encoder.int4range
  multirangeTypeName = "int4multirange"
  multirangeDecoder = Decoder.int4multirange
  multirangeEncoder = Encoder.int4multirange


instance DBRange Int64 where
  rangeTypeName = "int8range"
  rangeDecoder = Decoder.int8range
  rangeEncoder = Encoder.int8range
  multirangeTypeName = "int8multirange"
  multirangeDecoder = Decoder.int8multirange
  multirangeEncoder = Encoder.int8multirange


instance DBRange Scientific where
  rangeTypeName = "numrange"
  rangeDecoder = Decoder.numrange
  rangeEncoder = Encoder.numrange
  multirangeTypeName = "nummultirange"
  multirangeDecoder = Decoder.nummultirange
  multirangeEncoder = Encoder.nummultirange


instance DBRange LocalTime where
  rangeTypeName = "tsrange"
  rangeDecoder = Decoder.tsrange
  rangeEncoder = Encoder.tsrange
  multirangeTypeName = "tsmultirange"
  multirangeDecoder = Decoder.tsmultirange
  multirangeEncoder = Encoder.tsmultirange


instance DBRange UTCTime where
  rangeTypeName = "tstzrange"
  rangeDecoder = Decoder.tstzrange
  rangeEncoder = Encoder.tstzrange
  multirangeTypeName = "tstzmultirange"
  multirangeDecoder = Decoder.tstzmultirange
  multirangeEncoder = Encoder.tstzmultirange


instance DBRange Day where
  rangeTypeName = "daterange"
  rangeDecoder = Decoder.daterange
  rangeEncoder = Encoder.daterange
  multirangeTypeName = "datemultirange"
  multirangeDecoder = Decoder.datemultirange
  multirangeEncoder = Encoder.datemultirange
