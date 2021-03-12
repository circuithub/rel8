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
import Rel8.Kind.Emptiability ( Emptiability( Emptiable, NonEmptiable ) )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNonNullable, SNullable )
  )
import Rel8.Schema.Context ( Result( NullableResult, NonNullableResult ) )
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
import Rel8.Type.Array ( Array(..) )
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
      { htag = HIdentity (NonNullableResult IsLeft)
      , hleft = hnullify (const nullifier) table
      , hright = hnulls null
      }
    hright table = HEitherTable
      { htag = HIdentity (NonNullableResult IsRight)
      , hleft = hnulls null
      , hright = hnullify (const nullifier) table
      }


fromHEitherTable :: (HTable t, HTable u)
  => HEitherTable t u (H Result)
  -> Either (t (H Result)) (u (H Result))
fromHEitherTable HEitherTable {htag, hleft, hright} = case htag of
  HIdentity (NonNullableResult tag) -> case tag of
    IsLeft -> maybe err Left $ hunnullify unnullifier hleft
    IsRight -> maybe err Right $ hunnullify unnullifier hright
  where
    err = error "fromHEitherTable: mismatch between tag and data"


toHListTable :: HTable t => [t (H Result)] -> HListTable t (H Result)
toHListTable = hvectorize vectorizer


fromHListTable :: HTable t => HListTable t (H Result) -> [t (H Result)]
fromHListTable = hunvectorize unvectorizer


toHMaybeTable :: HTable t => Maybe (t (H Result)) -> HMaybeTable t (H Result)
toHMaybeTable = maybe hnothing hjust
  where
    hnothing = HMaybeTable
      { htag = HIdentity (NullableResult Nothing)
      , htable = hnulls null
      }
    hjust table = HMaybeTable
      { htag = HIdentity (NullableResult (Just IsJust))
      , htable = hnullify (const nullifier) table
      }


fromHMaybeTable :: HTable t => HMaybeTable t (H Result) -> Maybe (t (H Result))
fromHMaybeTable HMaybeTable {htag, htable} = case htag of
  HIdentity (NullableResult tag) ->
    tag *> hunnullify unnullifier htable


toHNonEmptyTable :: HTable t => NonEmpty (t (H Result)) -> HNonEmptyTable t (H Result)
toHNonEmptyTable = hvectorize vectorizer1


fromHNonEmptyTable :: HTable t => HNonEmptyTable t (H Result) -> NonEmpty (t (H Result))
fromHNonEmptyTable = hunvectorize unvectorizer1


toHTheseTable :: (HTable t, HTable u)
  => These (t (H Result)) (u (H Result))
  -> HTheseTable t u (H Result)
toHTheseTable tables = HTheseTable
  { hhere = toHMaybeTable (justHere tables)
  , hthere = toHMaybeTable (justThere tables)
  }


fromHTheseTable :: (HTable t, HTable u)
  => HTheseTable t u (H Result)
  -> These (t (H Result)) (u (H Result))
fromHTheseTable HTheseTable {hhere, hthere} =
  case (fromHMaybeTable hhere, fromHMaybeTable hthere) of
    (Just a, Nothing) -> This a
    (Nothing, Just b) -> That b
    (Just a, Just b) -> These a b
    _ -> error "fromHTheseTable: mismatch between tags and data"


null :: Result ('Spec defaulting 'Nullable a)
null = NullableResult Nothing


nullifier :: ()
  => Result ('Spec defaulting nullability a)
  -> Result ('Spec defaulting 'Nullable a)
nullifier result = case result of
  NonNullableResult a -> NullableResult (Just a)
  NullableResult ma -> NullableResult ma


unnullifier :: ()
  => SSpec ('Spec defaulting nullability a)
  -> Result ('Spec defaulting 'Nullable a)
  -> Maybe (Result ('Spec defaulting nullability a))
unnullifier (SSpec _ nullability _) (NullableResult ma) = case nullability of
  SNonNullable -> NonNullableResult <$> ma
  SNullable -> pure (NullableResult ma)


vectorizer :: ()
  => SSpec ('Spec defaulting nullability a)
  -> [Result ('Spec defaulting nullability a)]
  -> Result ('Spec defaulting 'NonNullable (Array 'Emptiable nullability a))
vectorizer (SSpec _ nullability _) results = case nullability of
  SNullable -> NonNullableResult $ NullableList $
    map (\(NullableResult a) -> a) results
  SNonNullable -> NonNullableResult $ NonNullableList $
    map (\(NonNullableResult a) -> a) results


vectorizer1 :: ()
  => SSpec ('Spec defaulting nullability a)
  -> NonEmpty (Result ('Spec defaulting nullability a))
  -> Result ('Spec defaulting 'NonNullable (Array 'NonEmptiable nullability a))
vectorizer1 (SSpec _ nullability _) results = case nullability of
  SNullable -> NonNullableResult $ NullableNonEmpty $
    fmap (\(NullableResult a) -> a) results
  SNonNullable -> NonNullableResult $ NonNullableNonEmpty $
    fmap (\(NonNullableResult a) -> a) results


unvectorizer :: ()
  => SSpec ('Spec defaulting nullability a)
  -> Result ('Spec defaulting 'NonNullable (Array 'Emptiable nullability a))
  -> [Result ('Spec defaulting nullability a)]
unvectorizer (SSpec _ nullability _) (NonNullableResult results) = case nullability of
  SNullable -> NullableResult <$> case results of
    NullableList as -> as
  SNonNullable -> NonNullableResult <$> case results of
    NonNullableList as -> as


unvectorizer1 :: ()
  => SSpec ('Spec defaulting nullability a)
  -> Result ('Spec defaulting 'NonNullable (Array 'NonEmptiable nullability a))
  -> NonEmpty (Result ('Spec defaulting nullability a))
unvectorizer1 (SSpec _ nullability _) (NonNullableResult results) = case nullability of
  SNullable -> NullableResult <$> case results of
    NullableNonEmpty as -> as
  SNonNullable -> NonNullableResult <$> case results of
    NonNullableNonEmpty as -> as
