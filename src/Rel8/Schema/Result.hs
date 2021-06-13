{-# language DataKinds #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Result
  ( Col( R, unR ), Result
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
    R :: {unR :: !a} -> Col Result ('Spec labels necessity a)


relabel :: ()
  => HIdentity ('Spec labels necessity a) (Col Result)
  -> HIdentity ('Spec relabels necessity a) (Col Result)
relabel (HIdentity (R a)) = HIdentity (R a)


null :: Col Result ('Spec labels necessity (Maybe a))
null = R Nothing


nullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Result ('Spec labels necessity a)
  -> Col Result ('Spec labels necessity (Nullify a))
nullifier SSpec {nullity} (R a) = R $ case nullity of
  Null -> a
  NotNull -> Just a


unnullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Result ('Spec labels necessity (Nullify a))
  -> Maybe (Col Result ('Spec labels necessity a))
unnullifier SSpec {nullity} (R a) =
  case nullity of
    Null -> pure $ R a
    NotNull -> R <$> a


vectorizer :: Functor f
  => SSpec ('Spec labels necessity a)
  -> f (Col Result ('Spec labels necessity a))
  -> Col Result ('Spec labels 'Required (f a))
vectorizer _ = R . fmap unR


unvectorizer :: Functor f
  => SSpec ('Spec labels necessity a)
  -> Col Result ('Spec labels 'Required (f a))
  -> f (Col Result ('Spec labels necessity a))
unvectorizer _ (R results) = R <$> results
