{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Schema.Result (
  Result,
  null,
  nullifier,
  unnullifier,
  vectorizer,
  unvectorizer,
)
where

-- base
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Prelude hiding (null)

-- rel8
import Rel8.Schema.Kind (Context)
import Rel8.Schema.Null (Nullify, Nullity (NotNull, Null))
import Rel8.Schema.Spec (Spec (..))


{- | The @Result@ context is the context used for decoded query results.

When a query is executed against a PostgreSQL database, Rel8 parses the
returned rows, decoding each row into the @Result@ context.
-}
type Result :: Context
type Result = Identity


null :: Result (Maybe a)
null = Identity Nothing


nullifier :: Spec a -> Result a -> Result (Nullify a)
nullifier Spec{nullity} (Identity a) = Identity $ case nullity of
  Null -> a
  NotNull -> Just a


unnullifier :: Spec a -> Result (Nullify a) -> Maybe (Result a)
unnullifier Spec{nullity} (Identity a) =
  case nullity of
    Null -> pure $ Identity a
    NotNull -> Identity <$> a


vectorizer :: Functor f => Spec a -> f (Result a) -> Result (f a)
vectorizer _ = Identity . fmap runIdentity


unvectorizer :: Functor f => Spec a -> Result (f a) -> f (Result a)
unvectorizer _ (Identity results) = Identity <$> results
