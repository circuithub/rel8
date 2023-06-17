{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language MultiWayIf #-}
{-# language OverloadedStrings #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Type
  ( DBType (typeInformation)
  )
where

-- aeson
import Data.Aeson ( Value )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>))
import Data.Int ( Int16, Int32, Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Kind ( Constraint, Type )
import Prelude

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy ( ByteString )
import qualified Data.ByteString.Lazy as ByteString ( fromStrict, toStrict )

-- case-insensitive
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

-- data-textual
import Data.Textual (textual)

-- hasql
import qualified Hasql.Decoders as Hasql

-- network-ip
import qualified Network.IP.Addr as IP

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye ( quote )

-- rel8
import Rel8.Schema.Null ( NotNull, Sql, nullable )
import Rel8.Type.Array ( listTypeInformation, nonEmptyTypeInformation )
import Rel8.Type.Decoder ( Decoder(..) )
import Rel8.Type.Information ( TypeInformation(..), mapTypeInformation )
import Rel8.Type.Name (TypeName (..))
import Rel8.Type.Parser (parse)
import Rel8.Type.Parser.ByteString (bytestring)
import qualified Rel8.Type.Parser.Time as Time

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (decodeUtf8)
import qualified Data.Text.Lazy as Lazy ( Text, unpack )
import qualified Data.Text.Lazy as Text ( fromStrict, toStrict )
import qualified Data.Text.Lazy.Encoding as Lazy ( decodeUtf8 )

-- time
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
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
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode =
        Decoder
          { binary = Hasql.bool
          , parser = \case
              "t" -> pure True
              "f" -> pure False
              input -> Left $ "bool: bad bool " <> show input
          , delimiter = ','
          }
    , typeName = "bool"
    }


-- | Corresponds to @char@
instance DBType Char where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . pure
    , typeName =
        TypeName
          { name = "bpchar"
          , modifiers = ["1"]
          , arrayDepth = 0
          }
    , decode = 
        Decoder
          { binary = Hasql.char
          , parser = \input -> case UTF8.uncons input of
              Just (char, rest) | BS.null rest -> pure char
              _ -> Left $ "char: bad char " <> show input
          , delimiter = ','
          }
    }


-- | Corresponds to @int2@
instance DBType Int16 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode =
        Decoder
          { binary = Hasql.int2
          , parser = parse (A.signed A.decimal)
          , delimiter = ','
          }
    , typeName = "int2"
    }


-- | Corresponds to @int4@
instance DBType Int32 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode =
        Decoder
          { binary = Hasql.int4
          , parser = parse (A.signed A.decimal)
          , delimiter = ','
          }
    , typeName = "int4"
    }


-- | Corresponds to @int8@
instance DBType Int64 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode =
        Decoder
          { binary = Hasql.int8
          , parser = parse (A.signed A.decimal)
          , delimiter = ','
          }
    , typeName = "int8"
    }


-- | Corresponds to @float4@
instance DBType Float where
  typeInformation = TypeInformation
    { encode = \x -> Opaleye.ConstExpr
        if | x == (1 / 0)  -> Opaleye.OtherLit "'Infinity'"
           | isNaN x       -> Opaleye.OtherLit "'NaN'"
           | x == (-1 / 0) -> Opaleye.OtherLit "'-Infinity'"
           | otherwise     -> Opaleye.NumericLit $ realToFrac x
    , decode =
        Decoder
          { binary = Hasql.float4
          , parser = parse (floating (realToFrac <$> A.double))
          , delimiter = ','
          }
    , typeName = "float4"
    }


-- | Corresponds to @float8@
instance DBType Double where
  typeInformation = TypeInformation
    { encode = \x -> Opaleye.ConstExpr
        if | x == (1 / 0)  -> Opaleye.OtherLit "'Infinity'"
           | isNaN x       -> Opaleye.OtherLit "'NaN'"
           | x == (-1 / 0) -> Opaleye.OtherLit "'-Infinity'"
           | otherwise     -> Opaleye.NumericLit $ realToFrac x
    , decode =
        Decoder
          { binary = Hasql.float8
          , parser = parse (floating A.double)
          , delimiter = ','
          }
    , typeName = "float8"
    }


-- | Corresponds to @numeric@
instance DBType Scientific where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit
    , decode =
        Decoder
          { binary = Hasql.numeric
          , parser = parse A.scientific
          , delimiter = ','
          }
    , typeName = "numeric"
    }


-- | Corresponds to @timestamptz@
instance DBType UTCTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%QZ'"
    , decode =
        Decoder
          { binary = Hasql.timestamptz
          , parser = parse Time.utcTime
          , delimiter = ','
          }
    , typeName = "timestamptz"
    }


-- | Corresponds to @date@
instance DBType Day where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%F'"
    , decode =
        Decoder
          { binary = Hasql.date
          , parser = parse Time.day
          , delimiter = ','
          }
    , typeName = "date"
    }


-- | Corresponds to @timestamp@
instance DBType LocalTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%Q'"
    , decode =
        Decoder
          { binary = Hasql.timestamp
          , parser = parse Time.localTime
          , delimiter = ','
          }
    , typeName = "timestamp"
    }


-- | Corresponds to @time@
instance DBType TimeOfDay where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%T%Q'"
    , decode =
        Decoder
          { binary = Hasql.time
          , parser = parse Time.timeOfDay
          , delimiter = ','
          }
    , typeName = "time"
    }


-- | Corresponds to @interval@
instance DBType CalendarDiffTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%bmon %0Es'"
    , decode =
        Decoder
          { binary = CalendarDiffTime 0 . realToFrac <$> Hasql.interval
          , parser = parse Time.calendarDiffTime
          , delimiter = ','
          }
    , typeName = "interval"
    }


-- | Corresponds to @text@
instance DBType Text where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . Text.unpack
    , decode =
        Decoder
          { binary = Hasql.text
          , parser = pure . Text.decodeUtf8
          , delimiter = ','
          }
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
    { encode = Opaleye.ConstExpr . Opaleye.ByteStringLit
    , decode =
        Decoder
          { binary = Hasql.bytea
          , parser = parse bytestring
          , delimiter = ','
          }
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
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . UUID.toString
    , decode =
        Decoder
          { binary = Hasql.uuid
          , parser = \input -> case UUID.fromASCIIBytes input of
              Just a -> pure a
              Nothing -> Left $ "uuid: bad UUID " <> show input
          , delimiter = ','
          }
    , typeName = "uuid"
    }


-- | Corresponds to @jsonb@
instance DBType Value where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        Opaleye.quote .
        Lazy.unpack . Lazy.decodeUtf8 . Aeson.encode
    , decode =
        Decoder
          { binary = Hasql.jsonb
          , parser = parse Aeson.value
          , delimiter = ','
          }
    , typeName = "jsonb"
    }


-- | Corresponds to @inet@
instance DBType (IP.NetAddr IP.IP) where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.StringLit . IP.printNetAddr
    , decode =
        Decoder
          { binary = Hasql.inet
          , parser = parse $
              textual
                <|> (`IP.netAddr` 32) . IP.IPv4 <$> textual
                <|> (`IP.netAddr` 128) . IP.IPv6 <$> textual
          , delimiter = ','
          }
    , typeName = "inet"
    }


instance Sql DBType a => DBType [a] where
  typeInformation = listTypeInformation nullable typeInformation


instance Sql DBType a => DBType (NonEmpty a) where
  typeInformation = nonEmptyTypeInformation nullable typeInformation


floating :: Floating a => A.Parser a -> A.Parser a
floating p = p <|> A.signed (1.0 / 0 <$ "Infinity") <|> 0.0 / 0 <$ "NaN"
