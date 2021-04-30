{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Result
  ( relabel
  , null, nullifier, unnullifier
  , vectorizer, unvectorizer
  )
where

-- base
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ) )
import Rel8.Schema.Result ( Col( Result ), Result )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )


relabel :: ()
  => HIdentity ('Spec labels necessity a) (Col Result)
  -> HIdentity ('Spec relabels necessity a) (Col Result)
relabel (HIdentity (Result a)) = HIdentity (Result a)


null :: Col Result ('Spec labels necessity (Maybe a))
null = Result Nothing


nullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Result ('Spec labels necessity a)
  -> Col Result ('Spec labels necessity (Nullify a))
nullifier SSpec {nullity} (Result a) = Result $ case nullity of
  Null -> a
  NotNull -> Just a


unnullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Result ('Spec labels necessity (Nullify a))
  -> Maybe (Col Result ('Spec labels necessity a))
unnullifier SSpec {nullity} (Result a) =
  case nullity of
    Null -> pure $ Result a
    NotNull -> Result <$> a


vectorizer :: Functor f
  => SSpec ('Spec labels necessity a)
  -> f (Col Result ('Spec labels necessity a))
  -> Col Result ('Spec labels necessity (f a))
vectorizer _ = Result . fmap (\(Result a) -> a)


unvectorizer :: Functor f
  => SSpec ('Spec labels necessity a)
  -> Col Result ('Spec labels necessity (f a))
  -> f (Col Result ('Spec labels necessity a))
unvectorizer _ (Result results) = Result <$> results
