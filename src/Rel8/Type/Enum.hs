{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Enum
  ( Enum( Enum )
  , DBEnum( enumValue, enumTypeName )
  , Enumable
  )
where

-- base
import Control.Applicative ( (<|>) )
import Control.Arrow ( (&&&) )
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics
  ( Generic, Rep, from, to
  , (:+:)( L1, R1 ), M1( M1 ), U1( U1 )
  , D, C, Meta( MetaCons )
  )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Prelude hiding ( Enum )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Schema.QualifiedName (QualifiedName)
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Decoder (Decoder (..))
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information ( TypeInformation(..) )
import Rel8.Type.Name (TypeName (..))
import Rel8.Type.Ord ( DBOrd, DBMax, DBMin )

-- text
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)


-- | A deriving-via helper type for column types that store an \"enum\" type
-- (in Haskell terms, a sum type where all constructors are nullary) using a
-- Postgres @enum@ type.
--
-- Note that this should map to a specific type in your database's schema
-- (explicitly created with @CREATE TYPE ... AS ENUM@). Use 'DBEnum' to
-- specify the name of this Postgres type and the names of the individual
-- values. If left unspecified, the names of the values of the Postgres
-- @enum@ are assumed to match exactly exactly the names of the constructors
-- of the Haskell type (up to and including case sensitivity).
type Enum :: Type -> Type
newtype Enum a = Enum
  { unEnum :: a
  }


instance DBEnum a => DBType (Enum a) where
  typeInformation = TypeInformation
    { decode =
        let
          mapping = (pack . enumValue &&& Enum) . to <$> genumerate @(Rep a)
          unrecognised = Left "enum: unrecognised value"
        in
          Decoder
            { binary = Hasql.enum (`lookup` mapping)
            , parser = maybe unrecognised pure . (`lookup` mapping) . decodeUtf8
            , delimiter = ','
            }
    , encode =
        Opaleye.ConstExpr .
        Opaleye.StringLit .
        enumValue @a .
        unEnum
    , typeName =
        TypeName
          { name = enumTypeName @a
          , modifiers = []
          , arrayDepth = 0
          }
    }


instance DBEnum a => DBEq (Enum a)


instance DBEnum a => DBOrd (Enum a)


instance DBEnum a => DBMax (Enum a)


instance DBEnum a => DBMin (Enum a)


-- | @DBEnum@ contains the necessary metadata to describe a PostgreSQL @enum@ type.
type DBEnum :: Type -> Constraint
class (DBType a, Enumable a) => DBEnum a where
  -- | Map Haskell values to the corresponding element of the @enum@ type. The
  -- default implementation of this method will use the exact name of the
  -- Haskell constructors.
  enumValue :: a -> String
  enumValue = gshow @(Rep a) . from

  -- | The name of the PostgreSQL @enum@ type that @a@ maps to.
  enumTypeName :: QualifiedName


-- | Types that are sum types, where each constructor is unary (that is, has no
-- fields).
type Enumable :: Type -> Constraint
class (Generic a, GEnumable (Rep a)) => Enumable a
instance (Generic a, GEnumable (Rep a)) => Enumable a


type GEnumable :: (Type -> Type) -> Constraint
class GEnumable rep where
  genumerate :: [rep x]
  gshow :: rep x -> String


instance GEnumable rep => GEnumable (M1 D meta rep) where
  genumerate = M1 <$> genumerate
  gshow (M1 rep) = gshow rep


instance (GEnumable a, GEnumable b) => GEnumable (a :+: b) where
  genumerate = L1 <$> genumerate <|> R1 <$> genumerate
  gshow = \case
    L1 a -> gshow a
    R1 a -> gshow a


instance
  ( meta ~ 'MetaCons name _fixity _isRecord
  , KnownSymbol name
  )
  => GEnumable (M1 C meta U1)
 where
  genumerate = [M1 U1]
  gshow (M1 U1) = symbolVal (Proxy @name)
