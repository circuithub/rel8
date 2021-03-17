{-# LANGUAGE TypeApplications #-}
{-# language KindSignatures #-}
{-# language StandaloneKindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Rel8.Type ( DBType(..), TypeInformation(..), mapTypeInformation, parseTypeInformation ) where

import Data.Kind (Constraint, Type)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Type.Decoder (Decoder, notNullDecoder)
import qualified Hasql.Decoders as Hasql
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Time (UTCTime, Day, LocalTime, TimeOfDay, DiffTime, NominalDiffTime, formatTime, defaultTimeLocale)
import Data.Text (Text)
import qualified Data.Text 
import qualified Data.Text.Lazy 
import qualified Data.Text.Lazy.Encoding
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye ( quote )
import Rel8.Type.Decoder (parseDecoder)


type DBType :: Type -> Constraint
class DBType a where
  typeInformation :: TypeInformation a


data TypeInformation (a :: Type) = TypeInformation
  { encode :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as a SQL expression.
  , typeName :: String
    -- ^ The name of the SQL type.
  , decode :: Decoder a
    -- ^ How to deserialize a single result back to Haskell.
  }


mapTypeInformation :: (a -> b) -> (b -> a) -> TypeInformation a -> TypeInformation b
mapTypeInformation aToB bToA TypeInformation{ encode, typeName, decode } = TypeInformation
  { encode = encode . bToA
  , decode = aToB <$> decode
  , typeName
  }


parseTypeInformation :: (a -> Either Text b) -> (b -> a) -> TypeInformation a -> TypeInformation b
parseTypeInformation aToB bToA TypeInformation{ encode, typeName, decode } = TypeInformation
  { encode = encode . bToA
  , decode = parseDecoder aToB decode
  , typeName
  }


instance DBType Bool where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode = notNullDecoder Hasql.bool
    , typeName = "bool"
    }


instance DBType Char where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . pure
    , decode = notNullDecoder Hasql.char
    , typeName = "char"
    }


instance DBType Int16 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = notNullDecoder Hasql.int2
    , typeName = "int2"
    }


instance DBType Int32 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = notNullDecoder Hasql.int4
    , typeName = "int4"
    }


instance DBType Int64 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = notNullDecoder Hasql.int8
    , typeName = "int8"
    }


instance DBType Float where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = notNullDecoder Hasql.float4
    , typeName = "float4"
    }


instance DBType Double where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = notNullDecoder Hasql.float8
    , typeName = "float8"
    }


instance DBType Scientific where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit
    , decode = notNullDecoder Hasql.numeric
    , typeName = "numeric"
    }


instance DBType UTCTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%QZ'"
    , decode = notNullDecoder Hasql.timestamptz
    , typeName = "timestamptz"
    }


instance DBType Day where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%F'"
    , decode = notNullDecoder Hasql.date
    , typeName = "date"
    }


instance DBType LocalTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%Q'"
    , decode = notNullDecoder Hasql.timestamp
    , typeName = "timestamp"
    }


instance DBType TimeOfDay where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%T%Q'"
    , decode = notNullDecoder Hasql.time
    , typeName = "time"
    }


instance DBType DiffTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%-6Es'"
    , decode = notNullDecoder Hasql.interval
    , typeName = "interval"
    }


instance DBType NominalDiffTime where
  typeInformation =
    mapTypeInformation @DiffTime realToFrac realToFrac typeInformation


instance DBType Text where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . Data.Text.unpack
    , decode = notNullDecoder Hasql.text
    , typeName = "text"
    }


instance DBType Data.Text.Lazy.Text where
  typeInformation =
    mapTypeInformation Data.Text.Lazy.fromStrict Data.Text.Lazy.toStrict typeInformation


instance DBType (CI Text) where
  typeInformation = mapTypeInformation CI.mk CI.original typeInformation
    { typeName = "citext"
    }


instance DBType (CI Data.Text.Lazy.Text) where
  typeInformation = mapTypeInformation CI.mk CI.original typeInformation
    { typeName = "citext"
    }


instance DBType ByteString where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.ByteStringLit
    , decode = notNullDecoder Hasql.bytea
    , typeName = "bytea"
    }


instance DBType Data.ByteString.Lazy.ByteString where
  typeInformation =
    mapTypeInformation Data.ByteString.Lazy.fromStrict Data.ByteString.Lazy.toStrict
      typeInformation


instance DBType UUID where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . UUID.toString
    , decode = notNullDecoder Hasql.uuid
    , typeName = "uuid"
    }


instance DBType Value where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        Opaleye.quote .
        Data.Text.Lazy.unpack . Data.Text.Lazy.Encoding.decodeUtf8 . Aeson.encode
    , decode = notNullDecoder Hasql.jsonb
    , typeName = "jsonb"
    }
