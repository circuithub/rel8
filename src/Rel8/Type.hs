{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
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
import Rel8.Schema.Nullability
  ( NotNull, Unnullify
  , HasNullability, nullabilization
  , Sql
  )
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
import Data.Time.Clock ( UTCTime )
import Data.Time.LocalTime
  ( CalendarDiffTime( CalendarDiffTime )
  , LocalTime
  , TimeOfDay
  )
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
type DBType :: Type -> Constraint
class NotNull a => DBType a where
  typeInformation :: TypeInformation a


-- | Corresponds to @bool@
instance DBType Bool where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode = Hasql.bool
    , typeName = "bool"
    }


-- | Corresponds to @char@
instance DBType Char where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . pure
    , decode = Hasql.char
    , typeName = "char"
    }


-- | Corresponds to @int2@
instance DBType Int16 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int2
    , typeName = "int2"
    }


-- | Corresponds to @int4@
instance DBType Int32 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int4
    , typeName = "int4"
    }


-- | Corresponds to @int8@
instance DBType Int64 where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int8
    , typeName = "int8"
    }


-- | Corresponds to @float4@
instance DBType Float where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float4
    , typeName = "float4"
    }


-- | Corresponds to @float8@
instance DBType Double where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float8
    , typeName = "float8"
    }


-- | Corresponds to @numeric@
instance DBType Scientific where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit
    , decode = Hasql.numeric
    , typeName = "numeric"
    }


-- | Corresponds to @timestamptz@
instance DBType UTCTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%QZ'"
    , decode = Hasql.timestamptz
    , typeName = "timestamptz"
    }


-- | Corresponds to @date@
instance DBType Day where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%F'"
    , decode = Hasql.date
    , typeName = "date"
    }


-- | Corresponds to @timestamp@
instance DBType LocalTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%Q'"
    , decode = Hasql.timestamp
    , typeName = "timestamp"
    }


-- | Corresponds to @time@
instance DBType TimeOfDay where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%T%Q'"
    , decode = Hasql.time
    , typeName = "time"
    }


-- | Corresponds to @interval@
instance DBType CalendarDiffTime where
  typeInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%bmon %0Es'"
    , decode = CalendarDiffTime 0 . realToFrac <$> Hasql.interval
    , typeName = "interval"
    }


-- | Corresponds to @text@
instance DBType Text where
  typeInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . Text.unpack
    , decode = Hasql.text
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
    , decode = Hasql.bytea
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
    , decode = Hasql.uuid
    , typeName = "uuid"
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
    }


instance Sql DBType a => DBType [a] where
  typeInformation = listTypeInformation nullabilization typeInformation


instance Sql DBType a => DBType (NonEmpty a) where
  typeInformation = nonEmptyTypeInformation nullabilization typeInformation


instance {-# OVERLAPPING #-} DBType Opaque where
  typeInformation = error "opaque"


instance {-# INCOHERENT #-} (HasNullability a, DBType (Unnullify a)) =>
  Sql DBType a
