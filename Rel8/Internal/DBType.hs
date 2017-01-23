{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rel8.Internal.DBType
  ( -- * @DBType@
    DBType(..)

    -- * Providing custom @DBType@s
  , TypeInfo(..)
  , showableDbType
  , typeInfoFromOpaleye
  , compositeDBType
  ) where

import Control.Category ((.))
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (toList)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import Data.Time (UTCTime, Day, LocalTime, TimeOfDay)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.FromField (FromField)
import Generics.OneLiner
       (For(..), ADTRecord, Constraints, gfoldMap)
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.PGTypes as O
import Prelude hiding (not, (.), id)

--------------------------------------------------------------------------------
-- | 'TypeInfo' records information about a database type - both how it can
-- safely formatted as a literal, and also the name of the type for casts.

data TypeInfo a = TypeInfo
  { formatLit :: a -> O.PrimExpr
  , dbTypeName :: String
  }

instance Contravariant TypeInfo where
  contramap f info = info { formatLit = formatLit info . f }

--------------------------------------------------------------------------------
-- Stock 'DBType's

-- | bool
instance DBType Bool where
  dbTypeInfo = typeInfoFromOpaleye O.pgBool

-- | char
instance DBType Char where
  dbTypeInfo = (typeInfoFromOpaleye (O.pgString . pure)) {dbTypeName = "char"}

-- | int2
instance DBType Int16 where
  dbTypeInfo =
    (typeInfoFromOpaleye (O.pgInt4 . fromIntegral)) {dbTypeName = "int2"}

-- | int4
instance DBType Int32 where
  dbTypeInfo = typeInfoFromOpaleye (O.pgInt4 . fromIntegral)

-- | int8
instance DBType Int64 where
  dbTypeInfo = typeInfoFromOpaleye O.pgInt8

-- | double
instance DBType Double where
  dbTypeInfo = typeInfoFromOpaleye O.pgDouble

-- | real
instance DBType Float where
  dbTypeInfo =
    (typeInfoFromOpaleye (O.pgDouble . realToFrac)) {dbTypeName = "real"}

-- | Columns that can take @null@. Has the same type name as the parent type.
instance DBType a =>
         DBType (Maybe a) where
  dbTypeInfo =
    TypeInfo
    { formatLit = maybe (O.ConstExpr O.NullLit) (formatLit dbTypeInfo)
    , dbTypeName = dbTypeName (dbTypeInfo @a)
    }

-- | text
instance DBType Text where
  dbTypeInfo = typeInfoFromOpaleye O.pgStrictText

-- | text
instance a ~ Char => DBType [a] where
  dbTypeInfo = contramap pack dbTypeInfo

-- | bytea
instance DBType ByteString where
  dbTypeInfo = typeInfoFromOpaleye O.pgStrictByteString

-- | timestamptz
instance DBType UTCTime where
  dbTypeInfo = typeInfoFromOpaleye O.pgUTCTime

-- | text
instance DBType LazyText.Text where
  dbTypeInfo = typeInfoFromOpaleye O.pgLazyText

-- | bytea
instance DBType LazyByteString.ByteString where
  dbTypeInfo = typeInfoFromOpaleye O.pgLazyByteString

-- | uuid
instance DBType UUID where
  dbTypeInfo = typeInfoFromOpaleye O.pgUUID

-- | date
instance DBType Day where
  dbTypeInfo = typeInfoFromOpaleye O.pgDay

-- | time
instance DBType TimeOfDay where
  dbTypeInfo = typeInfoFromOpaleye O.pgTimeOfDay

-- | timestamp
instance DBType LocalTime where
  dbTypeInfo = typeInfoFromOpaleye O.pgLocalTime

-- | numeric
instance DBType Scientific where
  dbTypeInfo =
    TypeInfo
    { formatLit = formatLit dbTypeInfo . toLazyText . scientificBuilder
    , dbTypeName = "numeric"
    }

-- | json
instance DBType Value where
  dbTypeInfo = typeInfoFromOpaleye O.pgValueJSON

instance (DBType a, Typeable a) =>
         DBType (Vector a) where
  dbTypeInfo =
    TypeInfo
    { formatLit =
        \xs ->
          O.unColumn
            (O.unsafeCast
               typeName
               (O.Column (O.ArrayExpr (map (formatLit elemInfo) (toList xs)))))
    , dbTypeName = typeName
    }
    where
      typeName = dbTypeName elemInfo ++ "[]"
      elemInfo = dbTypeInfo @a

--------------------------------------------------------------------------------
-- | The class of Haskell values that can be mapped to database types.
-- The @name@ argument specifies the name of the type in the database
-- schema.
--
-- By default, if @a@ has a 'Show' instance, we define 'dbTypeInfo' to use
-- 'showableDbType'.
class FromField a => DBType a where
  dbTypeInfo :: TypeInfo a

  default dbTypeInfo :: Show a => TypeInfo a
  dbTypeInfo = showableDbType


--------------------------------------------------------------------------------
-- | Map an @opaleye@ function that forms 'O.Column's into a @rel8@ 'TypeInfo'.

typeInfoFromOpaleye
  :: forall a b.
     O.IsSqlType b
  => (a -> O.Column b) -> TypeInfo a
typeInfoFromOpaleye f =
  TypeInfo {formatLit = O.unColumn . f, dbTypeName = O.showPGType (Proxy @b)}


--------------------------------------------------------------------------------
-- | Construct 'TypeInfo' for values that are stored in the database with
-- 'show'. It is assumed that the underlying field type is @text@ (though
-- you can change this by pattern matching on the resulting 'TypeInfo').

showableDbType :: (Show a) => TypeInfo a
showableDbType = contramap show dbTypeInfo


--------------------------------------------------------------------------------
-- | Show a type as a composite type. This is only valid for records, and
-- all fields in the record must be an instance of 'DBType'.
compositeDBType
  :: IsCompositeRecord a
  => String -- ^ The database schema name of the composite type
  -> TypeInfo a
compositeDBType n =
  TypeInfo
  { formatLit =
      catPrimExprs . gfoldMap (For :: For DBType) (pure . formatLit dbTypeInfo)
  , dbTypeName = n
  }
  where
    catPrimExprs :: [O.PrimExpr] -> O.PrimExpr
    catPrimExprs = O.FunExpr ""

class (ADTRecord a, Constraints a DBType) => IsCompositeRecord a
instance (ADTRecord a, Constraints a DBType) => IsCompositeRecord a
