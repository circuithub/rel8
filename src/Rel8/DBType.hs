{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}

module Rel8.DBType ( DBType(..) ) where

-- aeson
import Data.Aeson ( Value )

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
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
import Rel8.DBFunctor ( DBFunctor( liftDatabaseType ) )
import Rel8.DatabaseType ( DatabaseType, DatabaseType( DatabaseType ), decoder, encode, fromOpaleye, mapDatabaseType, nullDatabaseType, typeName )
import Rel8.DatabaseType.Decoder ( valueDecoder )

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
-- should be an instance of @DBType@ for all column types in your database
-- schema (e.g., @int@, @timestamptz@, etc).
-- 
-- Rel8 comes with stock instances for all default types in PostgreSQL, so you
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
  -- | Lookup the type information for the type @a@.
  typeInformation :: DatabaseType a


-- | Corresponds to the @json@ PostgreSQL type.
instance DBType Value where
  typeInformation = fromOpaleye pgValueJSON $ valueDecoder Hasql.json


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Text where
  typeInformation = fromOpaleye pgStrictText $ valueDecoder Hasql.text


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Data.Text.Lazy.Text where
  typeInformation = mapDatabaseType Data.Text.Lazy.fromStrict Data.Text.Lazy.toStrict typeInformation


-- | Corresponds to the @bool@ PostgreSQL type.
instance DBType Bool where
  typeInformation = fromOpaleye pgBool $ valueDecoder Hasql.bool


-- | Corresponds to the @int2@ PostgreSQL type.
instance DBType Int16 where
  typeInformation = (mapDatabaseType fromIntegral fromIntegral $ fromOpaleye pgInt4 $ fromIntegral <$> valueDecoder Hasql.int2) -- TODO
    { typeName = "int2" }


-- | Corresponds to the @int4@ PostgreSQL type.
instance DBType Int32 where
  typeInformation = mapDatabaseType fromIntegral fromIntegral $ fromOpaleye pgInt4 $ fromIntegral <$> valueDecoder Hasql.int4 -- TODO


-- | Corresponds to the @int8@ PostgreSQL type.
instance DBType Int64 where
  typeInformation = fromOpaleye pgInt8 $ valueDecoder Hasql.int8


instance DBType Float where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decoder = valueDecoder Hasql.float4
    , typeName = "float4"
    }


instance DBType UTCTime where
  typeInformation = fromOpaleye pgUTCTime $ valueDecoder Hasql.timestamptz


-- | Extends any @DBType@ with the value @null@. Note that you cannot "stack"
-- @Maybe@s, as SQL doesn't distinguish @Just Nothing@ from @Nothing@.
instance DBType a => DBType (Maybe a) where
  typeInformation = nullDatabaseType typeInformation


instance DBType Data.ByteString.Lazy.ByteString where
  typeInformation = mapDatabaseType Data.ByteString.Lazy.fromStrict Data.ByteString.Lazy.toStrict typeInformation


instance DBType Data.ByteString.ByteString where
  typeInformation = fromOpaleye pgStrictByteString $ valueDecoder Hasql.bytea


instance DBType Scientific where
  typeInformation = fromOpaleye pgNumeric $ valueDecoder Hasql.numeric


-- TODO
instance DBType Natural where
  typeInformation = mapDatabaseType round fromIntegral $ fromOpaleye pgNumeric $ valueDecoder Hasql.numeric


instance DBType Double where
  typeInformation = fromOpaleye pgDouble $ valueDecoder Hasql.float8


instance DBType UUID where
  typeInformation = fromOpaleye pgUUID $ valueDecoder Hasql.uuid


instance DBType Day where
  typeInformation = fromOpaleye pgDay $ valueDecoder Hasql.date


instance DBType LocalTime where
  typeInformation = fromOpaleye pgLocalTime $ valueDecoder Hasql.timestamp


instance DBType TimeOfDay where
  typeInformation = fromOpaleye pgTimeOfDay $ valueDecoder Hasql.time


instance DBType (CI Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }


instance DBType (CI Data.Text.Lazy.Text) where
  typeInformation = (mapDatabaseType CI.mk CI.original typeInformation) { typeName = "citext" }


instance DBType a => DBType [a] where
  typeInformation = liftDatabaseType typeInformation


instance DBType a => DBType (NonEmpty a) where
  typeInformation = liftDatabaseType typeInformation
