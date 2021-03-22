{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.PrimitiveType ( PrimitiveType(..) ) where

-- aeson
import Data.Aeson ( Value )

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Numeric.Natural ( Natural )

-- bytestring
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- case-insensitive
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Opaleye ( pgBool, pgDay, pgDouble, pgInt4, pgInt8, pgLocalTime, pgNumeric, pgStrictByteString, pgStrictText, pgTimeOfDay, pgUTCTime, pgUUID, pgValueJSON )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DatabaseType ( DatabaseType, DatabaseType( DatabaseType ), decoder, encode, fromOpaleye, mapDatabaseType, parser, typeName )
import Rel8.Nullify ( Nullify )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy

-- time
import Data.Time ( Day, LocalTime, TimeOfDay, UTCTime )

-- uuid
import Data.UUID ( UUID )


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
  typeInformation = fromOpaleye pgValueJSON Hasql.json


-- | Corresponds to the @text@ PostgreSQL type.
instance PrimitiveType Text where
  typeInformation = fromOpaleye pgStrictText Hasql.text


-- | Corresponds to the @text@ PostgreSQL type.
instance PrimitiveType Data.Text.Lazy.Text where
  typeInformation = mapDatabaseType Data.Text.Lazy.fromStrict Data.Text.Lazy.toStrict typeInformation


-- | Corresponds to the @bool@ PostgreSQL type.
instance PrimitiveType Bool where
  typeInformation = fromOpaleye pgBool Hasql.bool


-- | Corresponds to the @int2@ PostgreSQL type.
instance PrimitiveType Int16 where
  typeInformation = (mapDatabaseType fromIntegral fromIntegral $ fromOpaleye pgInt4 $ fromIntegral <$> Hasql.int2) -- TODO
    { typeName = "int2" }


-- | Corresponds to the @int4@ PostgreSQL type.
instance PrimitiveType Int32 where
  typeInformation = mapDatabaseType fromIntegral fromIntegral $ fromOpaleye pgInt4 $ fromIntegral <$> Hasql.int4 -- TODO


-- | Corresponds to the @int8@ PostgreSQL type.
instance PrimitiveType Int64 where
  typeInformation = fromOpaleye pgInt8 Hasql.int8


instance PrimitiveType Float where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decoder = Hasql.float4
    , typeName = "float4"
    , parser = pure
    }


instance PrimitiveType UTCTime where
  typeInformation = fromOpaleye pgUTCTime Hasql.timestamptz


instance PrimitiveType Data.ByteString.Lazy.ByteString where
  typeInformation = mapDatabaseType Data.ByteString.Lazy.fromStrict Data.ByteString.Lazy.toStrict typeInformation


instance PrimitiveType Data.ByteString.ByteString where
  typeInformation = fromOpaleye pgStrictByteString Hasql.bytea


instance PrimitiveType Scientific where
  typeInformation = fromOpaleye pgNumeric Hasql.numeric


-- TODO
instance PrimitiveType Natural where
  typeInformation = mapDatabaseType round fromIntegral $ fromOpaleye pgNumeric Hasql.numeric


instance PrimitiveType Double where
  typeInformation = fromOpaleye pgDouble Hasql.float8


instance PrimitiveType UUID where
  typeInformation = fromOpaleye pgUUID Hasql.uuid


instance PrimitiveType Day where
  typeInformation = fromOpaleye pgDay Hasql.date


instance PrimitiveType LocalTime where
  typeInformation = fromOpaleye pgLocalTime Hasql.timestamp


instance PrimitiveType TimeOfDay where
  typeInformation = fromOpaleye pgTimeOfDay Hasql.time


instance PrimitiveType (CI Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }


instance PrimitiveType (CI Data.Text.Lazy.Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }


