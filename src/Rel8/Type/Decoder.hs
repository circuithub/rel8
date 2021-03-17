{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language StandaloneKindSignatures #-}
{-# LANGUAGE GADTs #-}
module Rel8.Type.Decoder ( Decoder(..), notNullDecoder, parseDecoder ) where

import Data.Kind ( Type )
import qualified Hasql.Decoders as Hasql
import Control.Monad ((<=<))
import Data.Text (Text)

type Decoder :: Type -> Type
data Decoder a where
  DecodeNotNull :: Hasql.Value x -> (x -> a) -> Decoder a
  DecodeNull :: Hasql.Value x -> (Maybe x -> Either Text a) -> Decoder a


nullDecoder :: Hasql.Value a -> Decoder (Maybe a)
nullDecoder v = DecodeNull v pure


notNullDecoder :: Hasql.Value a -> Decoder a
notNullDecoder v = DecodeNotNull v id


instance Functor Decoder where
  fmap f (DecodeNotNull v g) = DecodeNotNull v (f . g)
  fmap f (DecodeNull v g) = DecodeNull v (fmap f . g)


-- | Enrich a 'DatabaseType' with the ability to parse @null@.
acceptNull :: Decoder a -> Decoder (Maybe a)
acceptNull = \case
  DecodeNotNull v f -> fmap f <$> nullDecoder v
  DecodeNull v f  -> DecodeNull v (fmap Just . f)


-- | Apply a parser to a decoder.
parseDecoder :: (a -> Either Text b) -> Decoder a -> Decoder b
parseDecoder f = \case
  DecodeNotNull v g -> DecodeNotNull (Hasql.refine (f . g) v) id
  DecodeNull v g    -> DecodeNull v (f <=< g)
