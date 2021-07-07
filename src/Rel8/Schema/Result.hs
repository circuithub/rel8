{-# language DataKinds #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Result
  ( Result( R, unR )
  , null, nullifier, unnullifier
  , vectorizer, unvectorizer
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context.Lower ( Lower )
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )


-- | The @Result@ context is the context used for decoded query results.
--
-- When a query is executed against a PostgreSQL database, Rel8 parses the
-- returned rows, decoding each row into the @Result@ context.
type Result :: Context
data Result a where
  R :: { unR :: !a } -> Result ('Spec a)


type instance Lower Result = Identity


null :: Result ('Spec (Maybe a))
null = R Nothing


nullifier :: ()
  => SSpec ('Spec a)
  -> Result ('Spec a)
  -> Result ('Spec (Nullify a))
nullifier SSpec {nullity} (R a) = R $ case nullity of
  Null -> a
  NotNull -> Just a


unnullifier :: ()
  => SSpec ('Spec a)
  -> Result ('Spec (Nullify a))
  -> Maybe (Result ('Spec a))
unnullifier SSpec {nullity} (R a) =
  case nullity of
    Null -> pure $ R a
    NotNull -> R <$> a


vectorizer :: Functor f
  => SSpec ('Spec a)
  -> f (Result ('Spec a))
  -> Result ('Spec (f a))
vectorizer _ = R . fmap unR


unvectorizer :: Functor f
  => SSpec ('Spec a)
  -> Result ('Spec (f a))
  -> f (Result ('Spec a))
unvectorizer _ (R results) = R <$> results
