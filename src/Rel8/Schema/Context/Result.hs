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
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( null )

-- rel8
import Rel8.Kind.Blueprint ( Blueprint( Vector ) )
import Rel8.Kind.Emptiability ( Emptiability( Emptiable, NonEmptiable ) )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNonNullable, SNullable )
  )
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
import Rel8.Schema.Spec ( Spec( Spec ), SSpec( SSpec ) )
import Rel8.Schema.Value ( Value( NullableValue, NonNullableValue ) )
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
      { htag = HIdentity (Result (NonNullableValue IsLeft))
      , hleft = hnullify (const nullifier) table
      , hright = hnulls null
      }
    hright table = HEitherTable
      { htag = HIdentity (Result (NonNullableValue IsRight))
      , hleft = hnulls null
      , hright = hnullify (const nullifier) table
      }
{-# INLINABLE toHEitherTable #-}


fromHEitherTable :: (HTable t, HTable u)
  => HEitherTable t u (H Result)
  -> Either (t (H Result)) (u (H Result))
fromHEitherTable HEitherTable {htag, hleft, hright} = case htag of
  HIdentity (Result (NonNullableValue tag)) -> case tag of
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
      { htag = HIdentity (Result (NullableValue Nothing))
      , hjust = hnulls null
      }
    hjust table = HMaybeTable
      { htag = HIdentity (Result (NullableValue (Just IsJust)))
      , hjust = hnullify (const nullifier) table
      }
{-# INLINABLE toHMaybeTable #-}


fromHMaybeTable :: HTable t => HMaybeTable t (H Result) -> Maybe (t (H Result))
fromHMaybeTable HMaybeTable {htag, hjust} = case htag of
  HIdentity (Result (NullableValue tag)) ->
    tag *> hunnullify unnullifier hjust
{-# INLINABLE fromHMaybeTable #-}


toHNonEmptyTable :: HTable t => NonEmpty (t (H Result)) -> HNonEmptyTable t (H Result)
toHNonEmptyTable = hvectorize vectorizer1
{-# INLINABLE toHNonEmptyTable #-}


fromHNonEmptyTable :: HTable t => HNonEmptyTable t (H Result) -> NonEmpty (t (H Result))
fromHNonEmptyTable = hunvectorize unvectorizer1
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


null :: Result ('Spec labels necessity 'Nullable a)
null = Result $ NullableValue Nothing


nullifier :: ()
  => Result ('Spec labels necessity nullability blueprint)
  -> Result ('Spec labels necessity 'Nullable blueprint)
nullifier (Result result) = Result $ case result of
  NonNullableValue a -> NullableValue (Just a)
  NullableValue ma -> NullableValue ma


unnullifier :: ()
  => SSpec ('Spec labels necessity nullability blueprint)
  -> Result ('Spec labels necessity 'Nullable blueprint)
  -> Maybe (Result ('Spec labels necessity nullability blueprint))
unnullifier (SSpec _ _ nullability _) (Result (NullableValue ma)) =
  case nullability of
    SNonNullable -> Result . NonNullableValue <$> ma
    SNullable -> pure (Result (NullableValue ma))


vectorizer :: ()
  => SSpec ('Spec labels necessity nullability blueprint)
  -> [Result ('Spec labels necessity nullability blueprint)]
  -> Result ('Spec labels necessity 'NonNullable ('Vector 'Emptiable nullability blueprint))
vectorizer (SSpec _ _ nullability _) results = case nullability of
  SNullable -> Result $ NonNullableValue $
    fmap (\(Result (NullableValue a)) -> a) results
  SNonNullable -> Result $ NonNullableValue $
    fmap (\(Result (NonNullableValue a)) -> a) results


vectorizer1 :: ()
  => SSpec ('Spec labels necessity nullability blueprint)
  -> NonEmpty (Result ('Spec labels necessity nullability blueprint))
  -> Result ('Spec labels necessity 'NonNullable ('Vector 'NonEmptiable nullability blueprint))
vectorizer1 (SSpec _ _ nullability _) results = case nullability of
  SNullable -> Result $ NonNullableValue $
    fmap (\(Result (NullableValue a)) -> a) results
  SNonNullable -> Result $ NonNullableValue $
    fmap (\(Result (NonNullableValue a)) -> a) results


unvectorizer :: ()
  => SSpec ('Spec labels necessity nullability blueprint)
  -> Result ('Spec labels necessity 'NonNullable ('Vector 'Emptiable nullability blueprint))
  -> [Result ('Spec labels necessity nullability blueprint)]
unvectorizer (SSpec _ _ nullability _) (Result (NonNullableValue results)) =
  case nullability of
    SNullable -> Result . NullableValue <$> results
    SNonNullable -> Result . NonNullableValue <$> results


unvectorizer1 :: ()
  => SSpec ('Spec labels necessity nullability blueprint)
  -> Result ('Spec labels necessity 'NonNullable ('Vector 'NonEmptiable nullability blueprint))
  -> NonEmpty (Result ('Spec labels necessity nullability blueprint))
unvectorizer1 (SSpec _ _ nullability _) (Result (NonNullableValue results)) =
  case nullability of
    SNullable -> Result . NullableValue <$> results
    SNonNullable -> Result . NonNullableValue <$> results


relabel :: ()
  => HIdentity ('Spec labels necessity nullability blueprint) (H Result)
  -> HIdentity ('Spec relabels necessity nullability blueprint) (H Result)
relabel (HIdentity (Result a)) = HIdentity (Result a)
