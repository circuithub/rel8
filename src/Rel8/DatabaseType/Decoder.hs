{-# language BlockArguments #-}
{-# language GADTs #-}
{-# language LambdaCase #-}

module Rel8.DatabaseType.Decoder
  ( HasqlDecoder(..)
  , acceptNull
  , parseDecoder
  , notNullDecoder
  , runHasqlDecoder
  , listDecoder
  ) where

-- base
import Control.Monad ( (<=<) )
import Data.Bifunctor ( first )

-- hasql
import qualified Hasql.Decoders as Hasql

-- text
import Data.Text ( pack )


data HasqlDecoder a where
  DecodeNotNull :: Hasql.Value x -> (x -> a) -> HasqlDecoder a
  DecodeNull :: Hasql.Value x -> (Maybe x -> Either String a) -> HasqlDecoder a


nullDecoder :: Hasql.Value a -> HasqlDecoder (Maybe a)
nullDecoder v = DecodeNull v pure


notNullDecoder :: Hasql.Value a -> HasqlDecoder a
notNullDecoder v = DecodeNotNull v id


instance Functor HasqlDecoder where
  fmap f (DecodeNotNull v g) = DecodeNotNull v (f . g)
  fmap f (DecodeNull v g) = DecodeNull v (fmap f . g)


-- | Enrich a 'DatabaseType' with the ability to parse @null@.
acceptNull :: HasqlDecoder a -> HasqlDecoder (Maybe a)
acceptNull = \case
  DecodeNotNull v f -> fmap f <$> nullDecoder v
  DecodeNull v f  -> DecodeNull v (fmap Just . f)


listDecoder :: HasqlDecoder a -> HasqlDecoder [a]
listDecoder = \case
  DecodeNotNull v f ->
    DecodeNotNull (Hasql.composite $ Hasql.field $ Hasql.nonNullable $ Hasql.listArray $ Hasql.nonNullable (f <$> v)) id

  DecodeNull v f -> DecodeNull v' \case
    Nothing -> pure <$> f Nothing
    Just xs -> traverse f xs
    where
      v' = Hasql.composite $ Hasql.field $ Hasql.nonNullable $ Hasql.listArray $ Hasql.nullable v


-- | Apply a parser to a decoder.
parseDecoder :: (a -> Either String b) -> HasqlDecoder a -> HasqlDecoder b
parseDecoder f = \case
  DecodeNotNull v g -> DecodeNotNull (Hasql.refine (first pack . f . g) v) id
  DecodeNull v g -> DecodeNull v (f <=< g)


runHasqlDecoder :: HasqlDecoder x -> Hasql.Row x
runHasqlDecoder = \case
  DecodeNotNull v f ->
    Hasql.column $ Hasql.nonNullable (f <$> v)

  DecodeNull v f ->
    either fail pure . f =<< Hasql.column (Hasql.nullable v)
