{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Type.Information (
  TypeInformation(..),
  mapTypeInformation,
  parseTypeInformation,
) where

-- base
import Data.Functor.Contravariant ((>$<))
import Data.Kind (Type)
import Prelude

-- rel8
import Rel8.Type.Decoder (Decoder, parseDecoder)
import Rel8.Type.Encoder (Encoder)
import Rel8.Type.Name (TypeName)


-- | @TypeInformation@ describes how to encode and decode a Haskell type to and
-- from database queries. The @typeName@ is the name of the type in the
-- database, which is used to accurately type literals. 
type TypeInformation :: Type -> Type
data TypeInformation a = TypeInformation
  { encode :: Encoder a
    -- ^ How to serialize a Haskell value to PostgreSQL.
  , decode :: Decoder a
    -- ^ How to deserialize a PostgreSQL result back to Haskell.
  , delimiter :: Char
    -- ^ The delimiter that is used in PostgreSQL's text format in arrays of
    -- this type (this is almost always ',').
  , typeName :: TypeName
    -- ^ The name of the SQL type.
  }
 

-- | Simultaneously map over how a type is both encoded and decoded, while
-- retaining the name of the type. This operation is useful if you want to
-- essentially @newtype@ another 'Rel8.DBType'.
-- 
-- The mapping is required to be total. If you have a partial mapping, see
-- 'parseTypeInformation'.
mapTypeInformation :: ()
  => (a -> b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
mapTypeInformation = parseTypeInformation . fmap pure


-- | Apply a parser to 'TypeInformation'.
-- 
-- This can be used if the data stored in the database should only be subset of
-- a given 'TypeInformation'. The parser is applied when deserializing rows
-- returned - the encoder assumes that the input data is already in the
-- appropriate form.
parseTypeInformation :: ()
  => (a -> Either String b) -> (b -> a)
  -> TypeInformation a -> TypeInformation b
parseTypeInformation to from TypeInformation {encode, decode, delimiter, typeName} =
  TypeInformation
    { decode = parseDecoder to decode
    , encode = from >$< encode
    , delimiter
    , typeName
    }
