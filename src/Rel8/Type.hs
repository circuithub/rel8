{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Type
  ( DBType(..)
  , TypeInformation(..)
  , mapTypeInformation
  , parseTypeInformation
  , withDBType
  , cast
  )
where

-- aeson
import Data.Aeson ( Value )
import qualified Data.Aeson as Aeson

-- base
import Data.Bifunctor ( first )
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Data.Typeable ( Typeable )
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

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
import Rel8.Schema.Dict ( Dict( Dict ) )

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


type TypeInformation :: Type -> Type
data TypeInformation (a :: Type) = TypeInformation
  { encode :: a -> Opaleye.PrimExpr
  , decode :: Hasql.Value a
  , typeName :: String
  , typeable :: Dict Typeable a
  }


mapTypeInformation :: forall a b. Typeable b
  => (a -> b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
mapTypeInformation = parseTypeInformation . fmap pure


parseTypeInformation :: forall a b. Typeable b
  => (a -> Either String b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
parseTypeInformation to from TypeInformation {encode, decode, typeName} =
  TypeInformation
    { encode = encode . from
    , decode = Hasql.refine (first Text.pack . to) decode
    , typeName
    , typeable = Dict
    }


type DBType :: Type -> Constraint
class DBType a where
  typeInformation :: TypeInformation a


instance DBType Bool where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode = Hasql.bool
    , typeName = "bool"
    , typeable = Dict
    }


instance DBType Char where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . pure
    , decode = Hasql.char
    , typeName = "char"
    , typeable = Dict
    }


instance DBType Int16 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int2
    , typeName = "int2"
    , typeable = Dict
    }


instance DBType Int32 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int4
    , typeName = "int4"
    , typeable = Dict
    }


instance DBType Int64 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int8
    , typeName = "int8"
    , typeable = Dict
    }


instance DBType Float where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float4
    , typeName = "float4"
    , typeable = Dict
    }


instance DBType Double where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float8
    , typeName = "float8"
    , typeable = Dict
    }


instance DBType Scientific where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit
    , decode = Hasql.numeric
    , typeName = "numeric"
    , typeable = Dict
    }


instance DBType UTCTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%QZ'"
    , decode = Hasql.timestamptz
    , typeName = "timestamptz"
    , typeable = Dict
    }


instance DBType Day where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%F'"
    , decode = Hasql.date
    , typeName = "date"
    , typeable = Dict
    }


instance DBType LocalTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%Q'"
    , decode = Hasql.timestamp
    , typeName = "timestamp"
    , typeable = Dict
    }


instance DBType TimeOfDay where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%T%Q'"
    , decode = Hasql.time
    , typeName = "time"
    , typeable = Dict
    }


instance DBType DiffTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%-6Es'"
    , decode = Hasql.interval
    , typeName = "interval"
    , typeable = Dict
    }


instance DBType NominalDiffTime where
  typeInformation =
    mapTypeInformation @DiffTime realToFrac realToFrac typeInformation


instance DBType Text where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . Text.unpack
    , decode = Hasql.text
    , typeName = "text"
    , typeable = Dict
    }


instance DBType Lazy.Text where
  typeInformation =
    mapTypeInformation Text.fromStrict Text.toStrict typeInformation


instance DBType (CI Text) where
  typeInformation = mapTypeInformation CI.mk CI.original typeInformation
    { typeName = "citext"
    }


instance DBType (CI Lazy.Text) where
  typeInformation = mapTypeInformation CI.mk CI.original typeInformation
    { typeName = "citext"
    }


instance DBType ByteString where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.ByteStringLit
    , decode = Hasql.bytea
    , typeName = "bytea"
    , typeable = Dict
    }


instance DBType Lazy.ByteString where
  typeInformation =
    mapTypeInformation ByteString.fromStrict ByteString.toStrict
      typeInformation


instance DBType UUID where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . UUID.toString
    , decode = Hasql.uuid
    , typeName = "uuid"
    , typeable = Dict
    }


instance DBType Value where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        Opaleye.quote .
        Lazy.unpack . Lazy.decodeUtf8 . Aeson.encode
    , decode = Hasql.jsonb
    , typeName = "jsonb"
    , typeable = Dict
    }


cast :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
cast = Opaleye.CastExpr . typeName


withDBType :: forall a b. TypeInformation a -> (DBType a => b) -> b
withDBType info f = unsafeCoerce (F f :: F a b) info


newtype F a b = F (DBType a => b)
