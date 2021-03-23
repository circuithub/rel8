{-# language FlexibleInstances #-}
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

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Kind ( Constraint, Type )
import Prelude

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )
import qualified Data.ByteString.Lazy as ByteString ( fromStrict, toStrict )

-- case-insensitive
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye ( quote )

-- rel8
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Nullability ( Nullabilizes, nullabilization )
import Rel8.Type.Array ( listTypeInformation, nonEmptyTypeInformation )
import Rel8.Type.Information ( TypeInformation(..), mapTypeInformation )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy ( Text, unpack )
import qualified Data.Text.Lazy as Text ( fromStrict, toStrict )
import qualified Data.Text.Lazy.Encoding as Lazy ( decodeUtf8 )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( DiffTime, NominalDiffTime, UTCTime )
import Data.Time.LocalTime ( LocalTime, TimeOfDay )
import Data.Time.Format ( formatTime, defaultTimeLocale )

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
-- 
-- [ Creating @DBType@s using @newtype@ ]
-- 
-- Generalized newtype deriving can be used when you want use a @newtype@
-- around a database type for clarity and accuracy in your Haskell code. A
-- common example is to @newtype@ row id types:
-- 
-- >>> newtype UserId = UserId { toInt32 :: Int32 } deriving newtype (DBType)
-- 
-- You can now write queries using @UserId@ instead of @Int32@, which may help
-- avoid making bad joins. However, when SQL is generated, it will be as if you
-- just used integers (the type distinction does not impact query generation).
type DBType :: Type -> Constraint
class DBType a where
  typeInformation :: TypeInformation a


-- | Corresponds to @bool@
instance DBType Bool where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode = Hasql.bool
    , typeName = "bool"
    , out = id
    }


-- | Corresponds to @char@
instance DBType Char where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . pure
    , decode = Hasql.char
    , typeName = "char"
    , out = id
    }


-- | Corresponds to @int2@
instance DBType Int16 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int2
    , typeName = "int2"
    , out = id
    }


-- | Corresponds to @int4@
instance DBType Int32 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int4
    , typeName = "int4"
    , out = id
    }


-- | Corresponds to @int8@
instance DBType Int64 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int8
    , typeName = "int8"
    , out = id
    }


-- | Corresponds to @float4@
instance DBType Float where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float4
    , typeName = "float4"
    , out = id
    }


-- | Corresponds to @float8@
instance DBType Double where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float8
    , typeName = "float8"
    , out = id
    }


-- | Corresponds to @numeric@
instance DBType Scientific where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit
    , decode = Hasql.numeric
    , typeName = "numeric"
    , out = id
    }


-- | Corresponds to @timestamptz@
instance DBType UTCTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%QZ'"
    , decode = Hasql.timestamptz
    , typeName = "timestamptz"
    , out = id
    }


-- | Corresponds to @date@
instance DBType Day where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%F'"
    , decode = Hasql.date
    , typeName = "date"
    , out = id
    }


-- | Corresponds to @timestamp@
instance DBType LocalTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%Q'"
    , decode = Hasql.timestamp
    , typeName = "timestamp"
    , out = id
    }


-- | Corresponds to @time@
instance DBType TimeOfDay where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%T%Q'"
    , decode = Hasql.time
    , typeName = "time"
    , out = id
    }


-- | Corresponds to @interval@
instance DBType DiffTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%-6Es'"
    , decode = Hasql.interval
    , typeName = "interval"
    , out = id
    }


instance DBType NominalDiffTime where
  typeInformation =
    mapTypeInformation @DiffTime realToFrac realToFrac typeInformation


-- | Corresponds to @text@
instance DBType Text where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . Text.unpack
    , decode = Hasql.text
    , typeName = "text"
    , out = id
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
    , decode = Hasql.bytea
    , typeName = "bytea"
    , out = id
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
    , decode = Hasql.uuid
    , typeName = "uuid"
    , out = id
    }


-- | Corresponds to @jsonb@
instance DBType Value where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        Opaleye.quote .
        Lazy.unpack . Lazy.decodeUtf8 . Aeson.encode
    , decode = Hasql.jsonb
    , typeName = "jsonb"
    , out = id
    }


instance (DBType db, Nullabilizes db a) => DBType [a] where
  typeInformation = listTypeInformation nullabilization typeInformation


instance (DBType db, Nullabilizes db a) => DBType (NonEmpty a) where
  typeInformation = nonEmptyTypeInformation nullabilization typeInformation


instance {-# OVERLAPPING #-} DBType Opaque where
  typeInformation = error "opaque"
