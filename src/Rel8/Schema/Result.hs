{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Result
  ( Col( R, unR ), Result
  , NotResult( NotResult ), absurd
  , null, nullifier, unnullifier
  , vectorizer, unvectorizer
  )
where

-- base
import Data.Kind ( Type )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context ( Interpretation( Col ) )
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )


-- | The @Result@ context is the context used for decoded query results.
--
-- When a query is executed against a PostgreSQL database, Rel8 parses the
-- returned rows, decoding each row into the @Result@ context.
type Result :: Context
data Result a


instance Interpretation Result where
  data Col Result _spec where
    R :: {unR :: !a} -> Col Result ('Spec a)


type IsResult :: Context -> Bool
type family IsResult context where
  IsResult Result = 'True
  IsResult _ = 'False


type NotResult :: Context -> Type
data NotResult context where
  NotResult :: IsResult context ~ 'False => NotResult context


absurd :: NotResult Result -> a
absurd = \case


null :: Col Result ('Spec (Maybe a))
null = R Nothing


nullifier :: ()
  => SSpec ('Spec a)
  -> Col Result ('Spec a)
  -> Col Result ('Spec (Nullify a))
nullifier SSpec {nullity} (R a) = R $ case nullity of
  Null -> a
  NotNull -> Just a


unnullifier :: ()
  => SSpec ('Spec a)
  -> Col Result ('Spec (Nullify a))
  -> Maybe (Col Result ('Spec a))
unnullifier SSpec {nullity} (R a) =
  case nullity of
    Null -> pure $ R a
    NotNull -> R <$> a


vectorizer :: Functor f
  => SSpec ('Spec a)
  -> f (Col Result ('Spec a))
  -> Col Result ('Spec (f a))
vectorizer _ = R . fmap unR


unvectorizer :: Functor f
  => SSpec ('Spec a)
  -> Col Result ('Spec (f a))
  -> f (Col Result ('Spec a))
unvectorizer _ (R results) = R <$> results
