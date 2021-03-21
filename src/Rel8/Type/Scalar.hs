{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Scalar
  ( DBScalar( scalarInformation )
  )
where

-- aeson
import Data.Aeson ( Value )
import qualified Data.Aeson as Aeson

-- base
import Data.Int ( Int16, Int32, Int64 )
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


type DBScalar :: Type -> Constraint
class DBScalar a where
  scalarInformation :: TypeInformation a


instance DBScalar Bool where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode = Hasql.bool
    , typeName = "bool"
    }


instance DBScalar Char where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . pure
    , decode = Hasql.char
    , typeName = "char"
    }


instance DBScalar Int16 where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int2
    , typeName = "int2"
    }


instance DBScalar Int32 where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int4
    , typeName = "int4"
    }


instance DBScalar Int64 where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger
    , decode = Hasql.int8
    , typeName = "int8"
    }


instance DBScalar Float where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float4
    , typeName = "float4"
    }


instance DBScalar Double where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = Hasql.float8
    , typeName = "float8"
    }


instance DBScalar Scientific where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit
    , decode = Hasql.numeric
    , typeName = "numeric"
    }


instance DBScalar UTCTime where
  scalarInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%QZ'"
    , decode = Hasql.timestamptz
    , typeName = "timestamptz"
    }


instance DBScalar Day where
  scalarInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%F'"
    , decode = Hasql.date
    , typeName = "date"
    }


instance DBScalar LocalTime where
  scalarInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%FT%T%Q'"
    , decode = Hasql.timestamp
    , typeName = "timestamp"
    }


instance DBScalar TimeOfDay where
  scalarInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%T%Q'"
    , decode = Hasql.time
    , typeName = "time"
    }


instance DBScalar DiffTime where
  scalarInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        formatTime defaultTimeLocale "'%-6Es'"
    , decode = Hasql.interval
    , typeName = "interval"
    }


instance DBScalar NominalDiffTime where
  scalarInformation =
    mapTypeInformation @DiffTime realToFrac realToFrac scalarInformation


instance DBScalar Text where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . Text.unpack
    , decode = Hasql.text
    , typeName = "text"
    }


instance DBScalar Lazy.Text where
  scalarInformation =
    mapTypeInformation Text.fromStrict Text.toStrict scalarInformation


instance DBScalar (CI Text) where
  scalarInformation = mapTypeInformation CI.mk CI.original scalarInformation
    { typeName = "citext"
    }


instance DBScalar (CI Lazy.Text) where
  scalarInformation = mapTypeInformation CI.mk CI.original scalarInformation
    { typeName = "citext"
    }


instance DBScalar ByteString where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.ByteStringLit
    , decode = Hasql.bytea
    , typeName = "bytea"
    }


instance DBScalar Lazy.ByteString where
  scalarInformation =
    mapTypeInformation ByteString.fromStrict ByteString.toStrict
      scalarInformation


instance DBScalar UUID where
  scalarInformation = TypeInformation
    { encode = Opaleye.ConstExpr . Opaleye.StringLit . UUID.toString
    , decode = Hasql.uuid
    , typeName = "uuid"
    }


instance DBScalar Value where
  scalarInformation = TypeInformation
    { encode =
        Opaleye.ConstExpr . Opaleye.OtherLit .
        Opaleye.quote .
        Lazy.unpack . Lazy.decodeUtf8 . Aeson.encode
    , decode = Hasql.jsonb
    , typeName = "jsonb"
    }


instance DBScalar Opaque where
  scalarInformation = error "opaque"
