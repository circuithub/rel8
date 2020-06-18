{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.DBType where

import Data.Profunctor ( Profunctor(..), dimap )
import Database.PostgreSQL.Simple.FromField ( FieldParser, fromField, optionalField )
import Data.Int
import Data.Kind
import Data.String
import Data.Text ( Text, unpack )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


{-| Haskell types that can be represented as expressiosn in a database. There
should be an instance of @DBType@ for all column types in your database
schema (e.g., @int@, @timestamptz@, etc).

Rel8 comes with stock instances for all default types in PostgreSQL.

[ @newtype@ing @DBType@s ]

Generalized newtype deriving can be used when you want use a @newtype@ around a
database type for clarity and accuracy in your Haskell code. A common example is
to @newtype@ row id types:

@
newtype UserId = UserId { toInt32 :: Int32 }
  deriving ( DBType )
@

You can now write queries using @UserId@ instead of @Int32@, which may help
avoid making bad joins. However, when SQL is generated, it will be as if you
just used integers (the type distinction does not impact query generation).

[ Using @Show@ with @DBType@ ]

@DBType@ also comes with a default instance using @Show@. This can be useful if
you have a small enumeration type that you need to store in your database, and
you're happy to just encode it as a string:

@
data Color = Red | Green | Blue | Purple | Gold
  deriving ( Show, DBType )
@

-}

class DBType ( a :: Type ) where
  typeInformation :: DatabaseType a a


data DatabaseType a b =
  DatabaseType
    { encode :: a -> Opaleye.PrimExpr
    , decode :: FieldParser b
    , typeName :: String
    }


instance Profunctor DatabaseType where
  dimap f g DatabaseType{ encode, decode, typeName } = DatabaseType
    { encode = encode . f
    , decode = \x y -> fmap g $ decode x y
    , typeName
    }


-- | Corresponds to the @bool@ PostgreSQL type.
instance DBType Bool where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode = fromField
    , typeName = "bool"
    }


-- | Corresponds to the @int4@ PostgreSQL type.
instance DBType Int32 where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral
    , decode = fromField
    , typeName = "int4"
    }


-- | Corresponds to the @int8@ PostgreSQL type.
instance DBType Int64 where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral
    , decode = fromField
    , typeName = "int8"
    }


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Text where
  typeInformation = dimap unpack fromString typeInformation


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType String where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.StringLit
    , decode = fromField
    , typeName = "text"
    }


-- | Extends any @DBType@ with the value @null@. Note that you cannot "stack"
-- @Maybe@s, as SQL doesn't distinguish @Just Nothing@ from @Nothing@.
instance DBType a => DBType ( Maybe a ) where
  typeInformation = DatabaseType
    { encode = maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
    , decode = optionalField decode
    , typeName
    }
    where
      DatabaseType{ encode, decode, typeName } = typeInformation
