{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MonoLocalBinds #-}
{-# language MultiWayIf #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Rel8.Type
  ( DBType (typeInformation)
  )
where

-- aeson
import Data.Aeson ( Value )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>))
import Data.Fixed (Fixed)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16, Int32, Int64)
import Data.List.NonEmpty ( NonEmpty )
import Data.Kind ( Constraint, Type )
import Prelude
import Text.Read (readMaybe)

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as Lazy ( ByteString )
import qualified Data.ByteString.Lazy as ByteString ( fromStrict, toStrict )
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim (primBounded)

-- case-insensitive
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

-- hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

-- iproute
import Data.IP (IPRange)

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye ( quote )

-- rel8
import Rel8.Schema.Null ( NotNull, Sql, nullable )
import Rel8.Type.Array ( listTypeInformation, nonEmptyTypeInformation )
import Rel8.Type.Decimal (PowerOf10, resolution)
import Rel8.Type.Decoder (Decoder (..))
import Rel8.Type.Encoder (Encoder (..))
import Rel8.Type.Information ( TypeInformation(..), mapTypeInformation )
import Rel8.Type.Name (TypeName (..))
import Rel8.Type.Parser (parse)
import qualified Rel8.Type.Builder.ByteString as Builder
import qualified Rel8.Type.Parser.ByteString as Parser
import qualified Rel8.Type.Builder.Time as Builder
import qualified Rel8.Type.Parser.Time as Parser

-- scientific
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.Scientific (Scientific)

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8Builder)
import qualified Data.Text.Lazy as Lazy (Text, unpack)
import qualified Data.Text.Lazy as Text (fromStrict, toStrict)

-- time
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.LocalTime
  ( CalendarDiffTime (CalendarDiffTime)
  , LocalTime
  , TimeOfDay
  )
import Data.Time.Format (formatTime, defaultTimeLocale)

-- utf8
import qualified Data.ByteString.UTF8 as UTF8

-- uuid
import Data.UUID ( UUID )
import qualified Data.UUID as UUID


-- | Haskell types that can be represented as expressions in a database. There
-- should be an instance of @DBType@ for all column types in your database
-- schema (e.g., @int@, @timestamptz@, etc).
-- 
-- Rel8 comes with stock instances for most default types in PostgreSQL, so you
-- should only need to derive instances of this class for custom database
-- types, such as types defined in PostgreSQL extensions, or custom domain
-- types.
type DBType :: Type -> Constraint
class NotNull a => DBType a where
  typeInformation :: TypeInformation a


-- | Corresponds to @bool@
instance DBType Bool where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.bool
          , text = \case
              False -> "f"
              True -> "t"
          , quote = Opaleye.ConstExpr . Opaleye.BoolLit
          }
    , decode =
        Decoder
          { binary = Decoders.bool
          , text = \case
              "t" -> pure True
              "f" -> pure False
              input -> Left $ "bool: bad bool " <> show input
          }
    , delimiter = ','
    , typeName = "bool"
    }


-- | Corresponds to @char@
instance DBType Char where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.char
          , text = B.charUtf8
          , quote = Opaleye.ConstExpr . Opaleye.StringLit . pure
          }
    , decode = 
        Decoder
          { binary = Decoders.char
          , text = \input -> case UTF8.uncons input of
              Just (char, rest) | BS.null rest -> pure char
              _ -> Left $ "char: bad char " <> show input
          }
    , delimiter = ','
    , typeName =
        TypeName
          { name = "bpchar"
          , modifiers = ["1"]
          , arrayDepth = 0
          }
    }


-- | Corresponds to @int2@
instance DBType Int16 where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.int2
          , text = B.int16Dec
          , quote = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
          }
    , decode =
        Decoder
          { binary = Decoders.int2
          , text = parse (A.signed A.decimal)
          }
    , delimiter = ','
    , typeName = "int2"
    }


-- | Corresponds to @int4@
instance DBType Int32 where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.int4
          , text = B.int32Dec
          , quote = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
          }
    , decode =
        Decoder
          { binary = Decoders.int4
          , text = parse (A.signed A.decimal)
          }
    , delimiter = ','
    , typeName = "int4"
    }


-- | Corresponds to @int8@
instance DBType Int64 where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.int8
          , text = B.int64Dec
          , quote = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
          }
    , decode =
        Decoder
          { binary = Decoders.int8
          , text = parse (A.signed A.decimal)
          }
    , delimiter = ','
    , typeName = "int8"
    }


-- | Corresponds to @float4@ and @real@
instance DBType Float where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.float4
          , text =
              \x ->
                if | x == (1 / 0)  -> "Infinity"
                   | isNaN x       -> "NaN"
                   | x == (-1 / 0) -> "-Infinity"
                   | otherwise     -> B.floatDec x
          , quote =
              \x -> Opaleye.ConstExpr
                if | x == (1 / 0)  -> Opaleye.OtherLit "'Infinity'"
                   | isNaN x       -> Opaleye.OtherLit "'NaN'"
                   | x == (-1 / 0) -> Opaleye.OtherLit "'-Infinity'"
                   | otherwise     -> Opaleye.DoubleLit $ realToFrac x
          }
    , decode =
        Decoder
          { binary = Decoders.float4
          , text = parse (floating (realToFrac <$> A.double))
          }
    , delimiter = ','
    , typeName = "float4"
    }


-- | Corresponds to @float8@ and @double precision@
instance DBType Double where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.float8
          , text =
              \x ->
                if | x == (1 / 0)  -> "Infinity"
                   | isNaN x       -> "NaN"
                   | x == (-1 / 0) -> "-Infinity"
                   | otherwise     -> B.doubleDec x
          , quote =
              \x -> Opaleye.ConstExpr
                if | x == (1 / 0)  -> Opaleye.OtherLit "'Infinity'"
                   | isNaN x       -> Opaleye.OtherLit "'NaN'"
                   | x == (-1 / 0) -> Opaleye.OtherLit "'-Infinity'"
                   | otherwise     -> Opaleye.DoubleLit x
          }
    , decode =
        Decoder
          { binary = Decoders.float8
          , text = parse (floating A.double)
          }
    , delimiter = ','
    , typeName = "float8"
    }


-- | Corresponds to @numeric@
instance DBType Scientific where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.numeric
          , text = scientificBuilder
          , quote = Opaleye.ConstExpr . Opaleye.NumericLit
          }
    , decode =
        Decoder
          { binary = Decoders.numeric
          , text = parse A.scientific
          }
    , delimiter = ','
    , typeName = "numeric"
    }


-- | Corresponds to @numeric(1000, log₁₀ n)@
instance PowerOf10 n => DBType (Fixed n) where
  typeInformation =
    mapTypeInformation realToFrac realToFrac (typeInformation @Scientific)
      { typeName =
          TypeName
            { name = "numeric"
            , modifiers = ["1000", show (resolution @n)]
            , arrayDepth = 0
            }
      }


-- | Corresponds to @timestamptz@
instance DBType UTCTime where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.timestamptz
          , text = primBounded Builder.utcTime
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit .
              formatTime defaultTimeLocale "'%FT%T%QZ'"
          }
    , decode =
        Decoder
          { binary = Decoders.timestamptz
          , text = parse Parser.utcTime
          }
    , delimiter = ','
    , typeName = "timestamptz"
    }


-- | Corresponds to @date@
instance DBType Day where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.date
          , text = primBounded Builder.day
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit .
              formatTime defaultTimeLocale "'%F'"
          }
    , decode =
        Decoder
          { binary = Decoders.date
          , text = parse Parser.day
          }
    , delimiter = ','
    , typeName = "date"
    }


-- | Corresponds to @timestamp@
instance DBType LocalTime where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.timestamp
          , text = primBounded Builder.localTime
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit .
              formatTime defaultTimeLocale "'%FT%T%Q'"
          }
    , decode =
        Decoder
          { binary = Decoders.timestamp
          , text = parse Parser.localTime
          }
    , delimiter = ','
    , typeName = "timestamp"
    }


-- | Corresponds to @time@
instance DBType TimeOfDay where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.time
          , text = primBounded Builder.timeOfDay
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit .
              formatTime defaultTimeLocale "'%T%Q'"
          }
    , decode =
        Decoder
          { binary = Decoders.time
          , text = parse Parser.timeOfDay
          }
    , delimiter = ','
    , typeName = "time"
    }


-- | Corresponds to @interval@
instance DBType CalendarDiffTime where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = toDiffTime >$< Encoders.interval
          , text = Builder.calendarDiffTime
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit .
              formatTime defaultTimeLocale "'%bmon %0Es'"
          }
    , decode =
        Decoder
          { binary = CalendarDiffTime 0 . realToFrac <$> Decoders.interval
          , text = parse Parser.calendarDiffTime
          }
    , delimiter = ','
    , typeName = "interval"
    }


-- | Corresponds to @text@
instance DBType Text where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.text
          , text = Text.encodeUtf8Builder
          , quote = Opaleye.ConstExpr . Opaleye.StringLit . Text.unpack
          }
    , decode =
        Decoder
          { binary = Decoders.text
          , text = pure . Text.decodeUtf8
          }
    , delimiter = ','
    , typeName = "text"
    }


-- | Corresponds to @text@
instance DBType Lazy.Text where
  typeInformation =
    mapTypeInformation Text.fromStrict Text.toStrict typeInformation


-- | Corresponds to @citext@
instance DBType (CI Text) where
  typeInformation = mapTypeInformation CI.mk CI.original typeInformation
    { typeName = "citext"
    }


-- | Corresponds to @citext@
instance DBType (CI Lazy.Text) where
  typeInformation = mapTypeInformation CI.mk CI.original typeInformation
    { typeName = "citext"
    }


-- | Corresponds to @bytea@
instance DBType ByteString where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.bytea
          , text = Builder.bytestring
          , quote = Opaleye.ConstExpr . Opaleye.ByteStringLit
          }
    , decode =
        Decoder
          { binary = Decoders.bytea
          , text = parse Parser.bytestring
          }
    , delimiter = ','
    , typeName = "bytea"
    }


-- | Corresponds to @bytea@
instance DBType Lazy.ByteString where
  typeInformation =
    mapTypeInformation ByteString.fromStrict ByteString.toStrict
      typeInformation


-- | Corresponds to @uuid@
instance DBType UUID where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.uuid
          , text = B.byteString . UUID.toASCIIBytes
          , quote = Opaleye.ConstExpr . Opaleye.StringLit . UUID.toString
          }
    , decode =
        Decoder
          { binary = Decoders.uuid
          , text = \input -> case UUID.fromASCIIBytes input of
              Just a -> pure a
              Nothing -> Left $ "uuid: bad UUID " <> show input
          }
    , delimiter = ','
    , typeName = "uuid"
    }


-- | Corresponds to @jsonb@
instance DBType Value where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.jsonb
          , text = Aeson.fromEncoding . Aeson.toEncoding
          , quote =
              Opaleye.ConstExpr . Opaleye.OtherLit . Opaleye.quote .
              Lazy.unpack . Aeson.encodeToLazyText
          }
    , decode =
        Decoder
          { binary = Decoders.jsonb
          , text = Aeson.eitherDecodeStrict
          }
    , delimiter = ','
    , typeName = "jsonb"
    }


-- | Corresponds to @inet@
instance DBType IPRange where
  typeInformation = TypeInformation
    { encode =
        Encoder
          { binary = Encoders.inet
          , text = B.string7 . show
          , quote = Opaleye.ConstExpr . Opaleye.StringLit . show
          }
    , decode =
        Decoder
          { binary = Decoders.inet
          , text = \str -> case readMaybe $ BS8.unpack str of
              Just x -> Right x
              Nothing -> Left "Failed to parse inet"
          }
    , delimiter = ','
    , typeName = "inet"
    }


instance Sql DBType a => DBType [a] where
  typeInformation = listTypeInformation nullable typeInformation


instance Sql DBType a => DBType (NonEmpty a) where
  typeInformation = nonEmptyTypeInformation nullable typeInformation


floating :: Floating a => A.Parser a -> A.Parser a
floating p = p <|> A.signed (1.0 / 0 <$ "Infinity") <|> 0.0 / 0 <$ "NaN"


toDiffTime :: CalendarDiffTime -> DiffTime
toDiffTime (CalendarDiffTime months seconds) =
  realToFrac (months * 30 * 24 * 60 * 60) + realToFrac seconds
