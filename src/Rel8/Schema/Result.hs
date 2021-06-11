{-# language DataKinds #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Result
  ( Col( Result ), Result
  , relabel
  , null, nullifier, unnullifier
  , vectorizer, unvectorizer
  )
where

-- base
import Prelude hiding ( null )

-- rel8
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.Context ( Interpretation( Col ) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )


type Result :: Context
data Result a


instance Interpretation Result where
  data Col Result _spec where
    Result :: a -> Col Result ('Spec labels necessity a)


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
  -> Col Result ('Spec labels 'Required (f a))
vectorizer _ = Result . fmap (\(Result a) -> a)


unvectorizer :: Functor f
  => SSpec ('Spec labels necessity a)
  -> Col Result ('Spec labels 'Required (f a))
  -> f (Col Result ('Spec labels necessity a))
unvectorizer _ (Result results) = Result <$> results
