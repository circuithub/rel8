{-# language BlockArguments #-}
{-# language GADTs #-}
{-# language LambdaCase #-}

module Rel8.DatabaseType.Decoder
  ( Decoder(..)
    -- * Construcing Decoders
  , valueDecoder

    -- * Running Decoders
  , runDecoder

    -- ** Transforming Decoders
  , parseDecoder
  , acceptNull
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


instance Functor Decoder where
  fmap f = \case
    DecodeNotNull v g -> DecodeNotNull v (f . g)
    DecodeNull v g    -> DecodeNull v (fmap f . g)


valueDecoder :: Hasql.Value a -> Decoder a
valueDecoder v = DecodeNotNull v id


-- | Enrich a 'DatabaseType' with the ability to parse @null@.
acceptNull :: Decoder a -> Decoder (Maybe a)
acceptNull = \case
  DecodeNotNull v f -> fmap f <$> nullDecoder
    where nullDecoder = DecodeNull v pure

  DecodeNull v f -> DecodeNull v (fmap Just . f)


listDecoder :: Decoder a -> Decoder [a]
listDecoder = \case
  DecodeNotNull v f -> DecodeNotNull v' id
    where v' = compositeArrayOf (Hasql.nonNullable (f <$> v))

  DecodeNull v f -> DecodeNull v' \case
    Nothing -> pure <$> f Nothing
    Just xs -> traverse f xs
    where v' = compositeArrayOf (Hasql.nullable v)
  where
    compositeArrayOf =
      Hasql.composite . Hasql.field . Hasql.nonNullable . Hasql.listArray


-- | Apply a parser to a decoder.
parseDecoder :: (a -> Either String b) -> Decoder a -> Decoder b
parseDecoder f = \case
  DecodeNotNull v g -> DecodeNotNull v' id
    where
      v' = Hasql.refine (first pack . f . g) v

  DecodeNull v g -> DecodeNull v (f <=< g)


runDecoder :: Decoder x -> Hasql.Row x
runDecoder = \case
  DecodeNotNull v f ->
    Hasql.column $ Hasql.nonNullable (f <$> v)

  DecodeNull v f ->
    either fail pure . f =<< Hasql.column (Hasql.nullable v)
