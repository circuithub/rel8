{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Result
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
where

-- base
import Data.Functor ( ($>) )
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context ( Result( Result ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Nullify ( hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.These ( HTheseTable(..) )
import Rel8.Schema.HTable.Vectorize ( hvectorize, hunvectorize )
import Rel8.Schema.Nullability
  ( Nullability( Nullable, NonNullable )
  )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ),  MaybeTag( IsJust ) )

-- these
import Data.These ( These( This, That, These ) )
import Data.These.Combinators ( justHere, justThere )


toHEitherTable :: (HTable t, HTable u)
  => Either (t (H Result)) (u (H Result))
  -> HEitherTable t u (H Result)
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
  => HEitherTable t u (H Result)
  -> Either (t (H Result)) (u (H Result))
fromHEitherTable HEitherTable {htag, hleft, hright} = case htag of
  HIdentity (Result tag) -> case tag of
    IsLeft -> maybe err Left $ hunnullify unnullifier hleft
    IsRight -> maybe err Right $ hunnullify unnullifier hright
  where
    err = error "fromHEitherTable: mismatch between tag and data"
{-# INLINABLE fromHEitherTable #-}


toHListTable :: HTable t => [t (H Result)] -> HListTable t (H Result)
toHListTable = hvectorize vectorizer
{-# INLINABLE toHListTable #-}


fromHListTable :: HTable t => HListTable t (H Result) -> [t (H Result)]
fromHListTable = hunvectorize unvectorizer
{-# INLINABLE fromHListTable #-}


toHMaybeTable :: HTable t => Maybe (t (H Result)) -> HMaybeTable t (H Result)
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


fromHMaybeTable :: HTable t => HMaybeTable t (H Result) -> Maybe (t (H Result))
fromHMaybeTable HMaybeTable {htag, hjust} = case htag of
  HIdentity (Result tag) -> tag $>
    case hunnullify unnullifier hjust of
      Nothing -> error "fromHMaybeTable: mismatch between tag and data"
      Just just -> just
{-# INLINABLE fromHMaybeTable #-}


toHNonEmptyTable :: HTable t => NonEmpty (t (H Result)) -> HNonEmptyTable t (H Result)
toHNonEmptyTable = hvectorize vectorizer
{-# INLINABLE toHNonEmptyTable #-}


fromHNonEmptyTable :: HTable t => HNonEmptyTable t (H Result) -> NonEmpty (t (H Result))
fromHNonEmptyTable = hunvectorize unvectorizer
{-# INLINABLE fromHNonEmptyTable #-}


toHTheseTable :: (HTable t, HTable u)
  => These (t (H Result)) (u (H Result))
  -> HTheseTable t u (H Result)
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
  => HTheseTable t u (H Result)
  -> These (t (H Result)) (u (H Result))
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


null :: Result ('Spec labels necessity db (Maybe db))
null = Result Nothing


nullifier :: ()
  => SSpec ('Spec labels necessity db a)
  -> Result ('Spec labels necessity db a)
  -> Result ('Spec labels necessity db (Maybe db))
nullifier SSpec {nullability} (Result a) = Result $ case nullability of
  Nullable -> a
  NonNullable -> Just a


unnullifier :: ()
  => SSpec ('Spec labels necessity db a)
  -> Result ('Spec labels necessity db (Maybe db))
  -> Maybe (Result ('Spec labels necessity db a))
unnullifier SSpec {nullability} (Result a) =
  case nullability of
    Nullable -> pure $ Result a
    NonNullable -> Result <$> a


vectorizer :: Functor f
  => SSpec ('Spec labels necessity db a)
  -> f (Result ('Spec labels necessity db a))
  -> Result ('Spec labels necessity (f a) (f a))
vectorizer _ = Result . fmap (\(Result a) -> a)


unvectorizer :: Functor f
  => SSpec ('Spec labels necessity db a)
  -> Result ('Spec labels necessity (f a) (f a))
  -> f (Result ('Spec labels necessity db a))
unvectorizer _ (Result results) = Result <$> results


relabel :: ()
  => HIdentity ('Spec labels necessity db a) (H Result)
  -> HIdentity ('Spec relabels necessity db a) (H Result)
relabel (HIdentity (Result a)) = HIdentity (Result a)
