{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.Decoder (
  Decoder (..),
  NullableOrNot (..),
  Parser,
  parseDecoder,
) where

-- base
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Prelude

-- bytestring
import Data.ByteString (ByteString)

-- hasql
import qualified Hasql.Decoders as Hasql

-- text
import qualified Data.Text as Text


type Parser :: Type -> Type
type Parser a = ByteString -> Either String a


type Decoder :: Type -> Type
data Decoder a = Decoder
  { binary :: Hasql.Value a
    -- ^ How to deserialize from PostgreSQL's binary format.
  , parser :: Parser a
    -- ^ How to deserialize from PostgreSQL's text format.
  , delimiter :: Char
    -- ^ The delimiter that is used in PostgreSQL's text format in arrays of
    -- this type (this is almost always ',').
  }
  deriving stock (Functor)


-- | Apply a parser to 'Decoder'.
--
-- This can be used if the data stored in the database should only be subset of
-- a given 'Decoder'. The parser is applied when deserializing rows
-- returned.
parseDecoder :: (a -> Either String b) -> Decoder a -> Decoder b
parseDecoder f Decoder {binary, parser, delimiter} =
  Decoder
    { binary = Hasql.refine (first Text.pack . f) binary
    , parser = parser >=> f
    , delimiter
    }


type NullableOrNot :: (Type -> Type) -> Type -> Type
data NullableOrNot decoder a where
  NonNullable :: decoder a -> NullableOrNot decoder a
  Nullable :: decoder a -> NullableOrNot decoder (Maybe a)
