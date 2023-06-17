{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Type.Information
  ( TypeInformation(..)
  , mapTypeInformation
  , parseTypeInformation
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Type.Name (TypeName)

-- text
import Rel8.Type.Decoder (Decoder, parseDecoder)


-- | @TypeInformation@ describes how to encode and decode a Haskell type to and
-- from database queries. The @typeName@ is the name of the type in the
-- database, which is used to accurately type literals. 
type TypeInformation :: Type -> Type
data TypeInformation a = TypeInformation
  { encode :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as a SQL expression.
  , decode :: Decoder a
    -- ^ How to deserialize a single result back to Haskell.
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
parseTypeInformation to from TypeInformation {encode, decode, typeName} =
  TypeInformation
    { encode = encode . from
    , decode = parseDecoder to decode
    , typeName
    }
