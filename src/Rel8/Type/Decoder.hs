{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.Decoder (
  Decoder (..),
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
  , text :: Parser a
    -- ^ How to deserialize from PostgreSQL's text format.
  }
  deriving stock (Functor)


-- | Apply a parser to 'Decoder'.
--
-- This can be used if the data stored in the database should only be subset of
-- a given 'Decoder'. The parser is applied when deserializing rows
-- returned.
parseDecoder :: (a -> Either String b) -> Decoder a -> Decoder b
parseDecoder f Decoder {binary, text} =
  Decoder
    { binary = Hasql.refine (first Text.pack . f) binary
    , text = text >=> f
    }

