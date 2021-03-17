{-# language BlockArguments #-}
{-# language GADTs #-}
{-# language LambdaCase #-}

module Rel8.DatabaseType.Decoder
  ( Decoder(..)
  , acceptNull
  , parseDecoder
  , notNullDecoder
  , runDecoder
  , listDecoder
  ) where

-- base
import Control.Monad ( (<=<) )
import Data.Bifunctor ( first )

-- hasql
import qualified Hasql.Decoders as Hasql

-- text
import Data.Text ( pack )


data Decoder a where
  DecodeNotNull :: Hasql.Value x -> (x -> a) -> Decoder a
  DecodeNull :: Hasql.Value x -> (Maybe x -> Either String a) -> Decoder a


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


listDecoder :: Decoder a -> Decoder [a]
listDecoder = \case
  DecodeNotNull v f ->
    DecodeNotNull (Hasql.composite $ Hasql.field $ Hasql.nonNullable $ Hasql.listArray $ Hasql.nonNullable (f <$> v)) id

  DecodeNull v f -> DecodeNull v' \case
    Nothing -> pure <$> f Nothing
    Just xs -> traverse f xs
    where
      v' = Hasql.composite $ Hasql.field $ Hasql.nonNullable $ Hasql.listArray $ Hasql.nullable v


-- | Apply a parser to a decoder.
parseDecoder :: (a -> Either String b) -> Decoder a -> Decoder b
parseDecoder f = \case
  DecodeNotNull v g -> DecodeNotNull (Hasql.refine (first pack . f . g) v) id
  DecodeNull v g -> DecodeNull v (f <=< g)


runDecoder :: Decoder x -> Hasql.Row x
runDecoder = \case
  DecodeNotNull v f ->
    Hasql.column $ Hasql.nonNullable (f <$> v)

  DecodeNull v f ->
    either fail pure . f =<< Hasql.column (Hasql.nullable v)
