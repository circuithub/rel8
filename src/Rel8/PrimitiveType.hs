{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.PrimitiveType ( PrimitiveType(..) ) where

-- aeson
import Data.Aeson ( Value )

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )

-- bytestring

-- case-insensitive
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import qualified Opaleye.Internal.HaskellDB.Sql.Default as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DatabaseType ( DatabaseType, DatabaseType( DatabaseType ), decoder, encode, mapDatabaseType, parser, typeName )
import Rel8.Nullify ( Nullify )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy

-- time
-- time
import Data.Time ( Day, LocalTime, TimeOfDay, UTCTime, formatTime, defaultTimeLocale )

-- uuid
import Data.UUID ( UUID )
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Aeson as Aeson
import qualified Data.Text as StrictText
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as StrictByteString
import qualified Data.UUID as UUID


-- | Haskell types that can be represented as expressions in a database. There
-- should be an instance of @PrimitiveType@ for all column types in your database
-- schema (e.g., @int@, @timestamptz@, etc).
-- 
-- Rel8 comes with stock instances for all default types in PostgreSQL, so you
-- should only need to derive instances of this class for custom database
-- types, such as types defined in PostgreSQL extensions, or custom domain
-- types.
-- 
-- [ Creating @PrimitiveType@s using @newtype@ ]
-- 
-- Generalized newtype deriving can be used when you want use a @newtype@
-- around a database type for clarity and accuracy in your Haskell code. A
-- common example is to @newtype@ row id types:
-- 
-- >>> newtype UserId = UserId { toInt32 :: Int32 } deriving newtype (PrimitiveType)
-- 
-- You can now write queries using @UserId@ instead of @Int32@, which may help
-- avoid making bad joins. However, when SQL is generated, it will be as if you
-- just used integers (the type distinction does not impact query generation).
type PrimitiveType :: Type -> Constraint


class Nullify a ~ Maybe a => PrimitiveType a where
  -- | Lookup the type information for the type @a@.
  typeInformation :: DatabaseType a


-- | Corresponds to the @json@ PostgreSQL type.
instance PrimitiveType Value where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.OtherLit . Opaleye.quote . LazyText.unpack . LazyText.decodeUtf8 . Aeson.encode
    , typeName = "json"
    , decoder = Hasql.json
    , parser = pure
    }


-- | Corresponds to the @text@ PostgreSQL type.
instance PrimitiveType Text where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . StrictText.unpack
    , decoder = Hasql.text
    , parser = pure
    , typeName = "text"
    }


-- | Corresponds to the @text@ PostgreSQL type.
instance PrimitiveType LazyText.Text where
  typeInformation = mapDatabaseType LazyText.fromStrict LazyText.toStrict typeInformation


-- | Corresponds to the @bool@ PostgreSQL type.
instance PrimitiveType Bool where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , typeName = "bool"
    , decoder = Hasql.bool
    , parser = pure
    }


-- | Corresponds to the @int2@ PostgreSQL type.
instance PrimitiveType Int16 where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral
    , decoder = Hasql.int2
    , typeName = "int2"
    , parser = pure
    }


-- | Corresponds to the @int4@ PostgreSQL type.
instance PrimitiveType Int32 where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral
    , decoder = Hasql.int4
    , typeName = "int4"
    , parser = pure
    }


-- | Corresponds to the @int8@ PostgreSQL type.
instance PrimitiveType Int64 where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral
    , decoder = Hasql.int8
    , typeName = "int8"
    , parser = pure
    }


instance PrimitiveType Float where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decoder = Hasql.float4
    , typeName = "float4"
    , parser = pure
    }


instance PrimitiveType UTCTime where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.OtherLit .  formatTime defaultTimeLocale "'%FT%T%QZ'"
    , decoder = Hasql.timestamptz
    , typeName = "timestamptz"
    , parser = pure
    }

instance PrimitiveType StrictByteString.ByteString where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.ByteStringLit
    , decoder = Hasql.bytea
    , typeName = "bytea"
    , parser = pure
    }


instance PrimitiveType LazyByteString.ByteString where
  typeInformation = mapDatabaseType LazyByteString.fromStrict LazyByteString.toStrict typeInformation


instance PrimitiveType Scientific where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit
    , decoder = Hasql.numeric
    , typeName = "numeric"
    , parser = pure
    }


instance PrimitiveType Double where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decoder = Hasql.float8
    , typeName = "float8"
    , parser = pure
    }


instance PrimitiveType UUID where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . UUID.toString
    , decoder = Hasql.uuid
    , typeName = "uuid"
    , parser = pure
    }


instance PrimitiveType Day where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.OtherLit .  formatTime defaultTimeLocale "'%F'"
    , decoder = Hasql.date
    , typeName = "date"
    , parser = pure
    }


instance PrimitiveType LocalTime where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.OtherLit .  formatTime defaultTimeLocale "'%FT%T%Q'"
    , decoder = Hasql.timestamp
    , typeName = "timestamp"
    , parser = pure
    }

instance PrimitiveType TimeOfDay where
   typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.OtherLit .  formatTime defaultTimeLocale "'%T%Q'"
    , decoder = Hasql.time
    , typeName = "time"
    , parser = pure
    }


instance PrimitiveType (CI Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }


instance PrimitiveType (CI Data.Text.Lazy.Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }
