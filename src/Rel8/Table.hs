{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table
  ( Table (Columns, Context)
  , toColumns, fromColumns
  , Congruent
  )
where

-- base
import Data.Functor ( ($>) )
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( null )

-- rel8
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Schema.Context ( Col(..) )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Nullify ( hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.These ( HTheseTable(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import Rel8.Schema.HTable.Vectorize ( hvectorize, hunvectorize )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ), Sql )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..), KnownSpec )
import Rel8.Type ( DBType )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ),  MaybeTag( IsJust ) )

-- these
import Data.These ( These( This, That, These ) )
import Data.These.Combinators ( justHere, justThere )


-- | @Table@s are one of the foundational elements of Rel8, and describe data
-- types that have a finite number of columns. Each of these columns contains
-- data under a shared context, and contexts describe how to interpret the
-- metadata about a column to a particular Haskell type. In Rel8, we have
-- contexts for expressions (the 'Rel8.Expr' context), aggregations (the
-- 'Rel8.Aggregate' context), insert values (the 'Rel8.Insert' contex), among
-- others.
--
-- In typical usage of Rel8 you don't need to derive instances of 'Table'
-- yourself, as anything that's an instance of 'Rel8.Rel8able' is always a
-- 'Table'.
type Table :: K.Context -> Type -> Constraint
class (HTable (Columns a), context ~ Context a) => Table context a | a -> context where
  -- | The 'HTable' functor that describes the schema of this table.
  type Columns a :: K.HTable

  -- | The common context that all columns use as an interpretation.
  type Context a :: K.Context

  toColumns :: a -> Columns a (Col (Context a))
  fromColumns :: Columns a (Col (Context a)) -> a


-- | Any 'HTable' is also a 'Table'.
instance HTable t => Table context (t (Col context)) where
  type Columns (t (Col context)) = t
  type Context (t (Col context)) = context

  toColumns = id
  fromColumns = id


-- | Any context is trivially a table.
instance KnownSpec spec => Table context (Col context spec) where
  type Columns (Col context spec) = HIdentity spec
  type Context (Col context spec) = context

  toColumns = HIdentity
  fromColumns = unHIdentity


instance Sql DBType a => Table Identity (Identity a) where
  type Columns (Identity a) = HType a
  type Context (Identity a) = Identity

  toColumns (Identity a) = HType (Result a)
  fromColumns (HType (Result a)) = Identity a


instance (Table Identity a, Table Identity b) => Table Identity (Either a b)
 where
  type Columns (Either a b) = HEitherTable (Columns a) (Columns b)
  type Context (Either a b) = Identity

  toColumns = \case
    Left table -> HEitherTable
      { htag = HIdentity (Result IsLeft)
      , hleft = hlabel labeler (hnullify nullifier (toColumns table))
      , hright = hlabel labeler (hnulls null)
      }
    Right table -> HEitherTable
      { htag = HIdentity (Result IsRight)
      , hleft = hlabel labeler (hnulls null)
      , hright = hlabel labeler (hnullify nullifier (toColumns table))
      }

  fromColumns HEitherTable {htag, hleft, hright} = case htag of
    HIdentity (Result tag) -> case tag of
      IsLeft -> maybe err (Left . fromColumns) $ hunnullify unnullifier (hunlabel unlabeler hleft)
      IsRight -> maybe err (Right . fromColumns) $ hunnullify unnullifier (hunlabel unlabeler hright)
    where
      err = error "Either.fromColumns: mismatch between tag and data"


instance Table Identity a => Table Identity [a] where
  type Columns [a] = HListTable (Columns a)
  type Context [a] = Identity

  toColumns = hvectorize vectorizer . fmap toColumns
  fromColumns = fmap fromColumns . hunvectorize unvectorizer


instance Table Identity a => Table Identity (Maybe a) where
  type Columns (Maybe a) = HMaybeTable (Columns a)
  type Context (Maybe a) = Identity

  toColumns = \case
    Nothing -> HMaybeTable
      { htag = HIdentity (Result Nothing)
      , hjust = hlabel labeler (hnulls null)
      }
    Just table -> HMaybeTable
      { htag = HIdentity (Result (Just IsJust))
      , hjust = hlabel labeler (hnullify nullifier (toColumns table))
      }

  fromColumns HMaybeTable {htag, hjust} = case htag of
    HIdentity (Result tag) -> tag $>
      case hunnullify unnullifier (hunlabel unlabeler hjust) of
        Nothing -> error "Maybe.fromColumns: mismatch between tag and data"
        Just just -> fromColumns just


instance Table Identity a => Table Identity (NonEmpty a) where
  type Columns (NonEmpty a) = HNonEmptyTable (Columns a)
  type Context (NonEmpty a) = Identity

  toColumns = hvectorize vectorizer . fmap toColumns
  fromColumns = fmap fromColumns . hunvectorize unvectorizer


instance (Table Identity a, Table Identity b) => Table Identity (These a b)
 where
  type Columns (These a b) = HTheseTable (Columns a) (Columns b)
  type Context (These a b) = Identity

  toColumns tables = HTheseTable
    { hhereTag = relabel hhereTag
    , hhere = hlabel labeler (hunlabel unlabeler (toColumns hhere))
    , hthereTag = relabel hthereTag
    , hthere = hlabel labeler (hunlabel unlabeler (toColumns hthere))
    }
    where
      HMaybeTable
        { htag = hhereTag
        , hjust = hhere
        } = toColumns (justHere tables)
      HMaybeTable
        { htag = hthereTag
        , hjust = hthere
        } = toColumns (justThere tables)

  fromColumns HTheseTable {hhereTag, hhere, hthereTag, hthere} =
    case (fromColumns mhere, fromColumns mthere) of
      (Just a, Nothing) -> This (fromColumns a)
      (Nothing, Just b) -> That (fromColumns b)
      (Just a, Just b) -> These (fromColumns a) (fromColumns b)
      _ -> error "These.fromColumns: mismatch between tags and data"
    where
      mhere = HMaybeTable
        { htag = relabel hhereTag
        , hjust = hlabel labeler (hunlabel unlabeler hhere)
        }
      mthere = HMaybeTable
        { htag = relabel hthereTag
        , hjust = hlabel labeler (hunlabel unlabeler hthere)
        }


instance
  ( Table context a, Table context b
  , Labelable context
  ) =>
  Table context (a, b)
 where
  type Columns (a, b) =
    HPair
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
  type Context (a, b) = Context a

  toColumns (a, b) = HPair
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    }
  fromColumns (HPair a b) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    )


instance
  ( Table context a, Table context b, Table context c
  , Labelable context
  ) => Table context (a, b, c)
 where
  type Columns (a, b, c) =
    HTrio
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
      (HLabel "trd" (Columns c))
  type Context (a, b, c) = Context a

  toColumns (a, b, c) = HTrio
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    , htrd = hlabel labeler $ toColumns c
    }
  fromColumns (HTrio a b c) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    , fromColumns $ hunlabel unlabeler c
    )


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Labelable context
  ) => Table context (a, b, c, d)
 where
  type Columns (a, b, c, d) =
    HQuartet
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
      (HLabel "trd" (Columns c))
      (HLabel "frt" (Columns d))
  type Context (a, b, c, d) = Context a

  toColumns (a, b, c, d) = HQuartet
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    , htrd = hlabel labeler $ toColumns c
    , hfrt = hlabel labeler $ toColumns d
    }
  fromColumns (HQuartet a b c d) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    , fromColumns $ hunlabel unlabeler c
    , fromColumns $ hunlabel unlabeler d
    )


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e
  , Labelable context
  ) => Table context (a, b, c, d, e)
 where
  type Columns (a, b, c, d, e) =
    HQuintet
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
      (HLabel "trd" (Columns c))
      (HLabel "frt" (Columns d))
      (HLabel "fft" (Columns e))
  type Context (a, b, c, d, e) = Context a

  toColumns (a, b, c, d, e) = HQuintet
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    , htrd = hlabel labeler $ toColumns c
    , hfrt = hlabel labeler $ toColumns d
    , hfft = hlabel labeler $ toColumns e
    }
  fromColumns (HQuintet a b c d e) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    , fromColumns $ hunlabel unlabeler c
    , fromColumns $ hunlabel unlabeler d
    , fromColumns $ hunlabel unlabeler e
    )


type Congruent :: Type -> Type -> Constraint
class Columns a ~ Columns b => Congruent a b
instance Columns a ~ Columns b => Congruent a b


null :: Col Identity ('Spec labels necessity (Maybe a))
null = Result Nothing


nullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Identity ('Spec labels necessity a)
  -> Col Identity ('Spec labels necessity (Nullify a))
nullifier SSpec {nullity} (Result a) = Result $ case nullity of
  Null -> a
  NotNull -> Just a


unnullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Col Identity ('Spec labels necessity (Nullify a))
  -> Maybe (Col Identity ('Spec labels necessity a))
unnullifier SSpec {nullity} (Result a) =
  case nullity of
    Null -> pure $ Result a
    NotNull -> Result <$> a


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
