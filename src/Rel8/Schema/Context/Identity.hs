{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Identity
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
where

-- base
import Data.Functor ( ($>) )
import Data.Functor.Identity ( Identity )
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context ( Col( Result ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Nullify ( hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.These ( HTheseTable(..) )
import Rel8.Schema.HTable.Vectorize ( hvectorize, hunvectorize )
import Rel8.Schema.Nullability
  ( Nullify
  , Nullability( Nullable, NonNullable )
  )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ),  MaybeTag( IsJust ) )

-- these
import Data.These ( These( This, That, These ) )
import Data.These.Combinators ( justHere, justThere )


toHEitherTable :: (HTable t, HTable u)
  => Either (t (Col Identity)) (u (Col Identity))
  -> HEitherTable t u (Col Identity)
toHEitherTable = either hleft hright
  where
    hleft table = HEitherTable
      { htag = HIdentity (Result IsLeft)
      , hleft = hnullify nullifier table
      , hright = hnulls null
      }
    hright table = HEitherTable
      { htag = HIdentity (Result IsRight)
      , hleft = hnulls null
      , hright = hnullify nullifier table
      }
{-# INLINABLE toHEitherTable #-}


fromHEitherTable :: (HTable t, HTable u)
  => HEitherTable t u (Col Identity)
  -> Either (t (Col Identity)) (u (Col Identity))
fromHEitherTable HEitherTable {htag, hleft, hright} = case htag of
  HIdentity (Result tag) -> case tag of
    IsLeft -> maybe err Left $ hunnullify unnullifier hleft
    IsRight -> maybe err Right $ hunnullify unnullifier hright
  where
    err = error "fromHEitherTable: mismatch between tag and data"
{-# INLINABLE fromHEitherTable #-}


toHListTable :: HTable t => [t (Col Identity)] -> HListTable t (Col Identity)
toHListTable = hvectorize vectorizer
{-# INLINABLE toHListTable #-}


fromHListTable :: HTable t => HListTable t (Col Identity) -> [t (Col Identity)]
fromHListTable = hunvectorize unvectorizer
{-# INLINABLE fromHListTable #-}


toHMaybeTable :: HTable t => Maybe (t (Col Identity)) -> HMaybeTable t (Col Identity)
toHMaybeTable = maybe hnothing hjust
  where
    hnothing = HMaybeTable
      { htag = HIdentity (Result Nothing)
      , hjust = hnulls null
      }
    hjust table = HMaybeTable
      { htag = HIdentity (Result (Just IsJust))
      , hjust = hnullify nullifier table
      }
{-# INLINABLE toHMaybeTable #-}


fromHMaybeTable :: HTable t => HMaybeTable t (Col Identity) -> Maybe (t (Col Identity))
fromHMaybeTable HMaybeTable {htag, hjust} = case htag of
  HIdentity (Result tag) -> tag $>
    case hunnullify unnullifier hjust of
      Nothing -> error "fromHMaybeTable: mismatch between tag and data"
      Just just -> just
{-# INLINABLE fromHMaybeTable #-}


toHNonEmptyTable :: HTable t => NonEmpty (t (Col Identity)) -> HNonEmptyTable t (Col Identity)
toHNonEmptyTable = hvectorize vectorizer
{-# INLINABLE toHNonEmptyTable #-}


fromHNonEmptyTable :: HTable t => HNonEmptyTable t (Col Identity) -> NonEmpty (t (Col Identity))
fromHNonEmptyTable = hunvectorize unvectorizer
{-# INLINABLE fromHNonEmptyTable #-}


toHTheseTable :: (HTable t, HTable u)
  => These (t (Col Identity)) (u (Col Identity))
  -> HTheseTable t u (Col Identity)
toHTheseTable tables = HTheseTable
  { hhereTag = relabel hhereTag
  , hhere
  , hthereTag = relabel hthereTag
  , hthere
  }
  where
    HMaybeTable
      { htag = hhereTag
      , hjust = hhere
      } = toHMaybeTable (justHere tables)
    HMaybeTable
      { htag = hthereTag
      , hjust = hthere
      } = toHMaybeTable (justThere tables)
{-# INLINABLE toHTheseTable #-}


fromHTheseTable :: (HTable t, HTable u)
  => HTheseTable t u (Col Identity)
  -> These (t (Col Identity)) (u (Col Identity))
fromHTheseTable HTheseTable {hhereTag, hhere, hthereTag, hthere} =
  case (fromHMaybeTable mhere, fromHMaybeTable mthere) of
    (Just a, Nothing) -> This a
    (Nothing, Just b) -> That b
    (Just a, Just b) -> These a b
    _ -> error "fromHTheseTable: mismatch between tags and data"
  where
    mhere = HMaybeTable
      { htag = relabel hhereTag
      , hjust = hhere
      }
    mthere = HMaybeTable
      { htag = relabel hthereTag
      , hjust = hthere
      }
{-# INLINABLE fromHTheseTable #-}


null :: Col Identity ('Spec labels necessity (Maybe a))
null = Result Nothing


nullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Identity ('Spec labels necessity a)
  -> Col Identity ('Spec labels necessity (Nullify a))
nullifier SSpec {nullability} (Result a) = Result $ case nullability of
  Nullable -> a
  NonNullable -> Just a


unnullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Identity ('Spec labels necessity (Nullify a))
  -> Maybe (Col Identity ('Spec labels necessity a))
unnullifier SSpec {nullability} (Result a) =
  case nullability of
    Nullable -> pure $ Result a
    NonNullable -> Result <$> a


vectorizer :: Functor f
  => SSpec ('Spec labels necessity a)
  -> f (Col Identity ('Spec labels necessity a))
  -> Col Identity ('Spec labels necessity (f a))
vectorizer _ = Result . fmap (\(Result a) -> a)


unvectorizer :: Functor f
  => SSpec ('Spec labels necessity a)
  -> Col Identity ('Spec labels necessity (f a))
  -> f (Col Identity ('Spec labels necessity a))
unvectorizer _ (Result results) = Result <$> results


relabel :: ()
  => HIdentity ('Spec labels necessity a) (Col Identity)
  -> HIdentity ('Spec relabels necessity a) (Col Identity)
relabel (HIdentity (Result a)) = HIdentity (Result a)
