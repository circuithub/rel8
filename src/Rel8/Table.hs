{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table
  ( Table
      ( Columns, Context, toColumns, fromColumns
      , Unreify, reify, unreify, coherence, congruence
      )
  , Congruent
  , TTable, TColumns, TContext, TUnreify
  )
where

-- base
import Data.Functor ( ($>) )
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude hiding ( null )

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Kind.Algebra
  ( SAlgebra( SSum, SProduct )
  , KnownAlgebra, algebraSing
  )
import Rel8.Generic.Coherence ( GCoherent, gcoherence, gcongruence )
import Rel8.Generic.Map ( GMap, GMappable, gmap, gunmap )
import Rel8.Generic.Table
  ( GGTable, GGColumns, GGContext, ggfromColumns, ggtoColumns
  , GAlgebra
  )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Generic.Reify ( ARep )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..), HType )
import Rel8.Schema.HTable.Label ( hlabel, hrelabel, hunlabel )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Nullify ( hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.These ( HTheseTable(..) )
import Rel8.Schema.HTable.Vectorize ( hvectorize, hunvectorize )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify
  ( Reify, Col( Reify ), hreify, hunreify
  , UnwrapReify
  , notReify
  )
import Rel8.Schema.Result
  ( Col( R ), Result
  , NotResult
  , null, nullifier, unnullifier
  , vectorizer, unvectorizer
  )
import Rel8.Schema.Spec ( KnownSpec )
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

  type Unreify a :: Type

  toColumns :: a -> Columns a (Col context)
  fromColumns :: Columns a (Col context) -> a

  reify :: context :~: Reify ctx -> Unreify a -> a
  unreify :: context :~: Reify ctx -> a -> Unreify a
  coherence :: context :~: Reify ctx -> NotResult ctx -> Context (Unreify a) :~: ctx
  congruence :: context :~: Reify ctx -> NotResult ctx -> Columns a :~: Columns (Unreify a)


  type Columns a = Eval (GGColumns (GAlgebra (Rep (Record a))) TColumns (Rep (Record a)))
  type Context a = Eval (GGContext (GAlgebra (Rep (Record a))) TContext (Rep (Record a)))
  type Unreify a = DefaultUnreify a

  default toColumns ::
    ( Generic (Record a)
    , KnownAlgebra (GAlgebra (Rep (Record a)))
    , Eval (GGTable (GAlgebra (Rep (Record a))) (TTable context) TColumns (Col context) (Rep (Record a)))
    , Columns a ~ Eval (GGColumns (GAlgebra (Rep (Record a))) TColumns (Rep (Record a)))
    , Context a ~ Eval (GGContext (GAlgebra (Rep (Record a))) TContext (Rep (Record a)))
    )
    => a -> Columns a (Col context)
  toColumns =
    ggtoColumns
      @(GAlgebra (Rep (Record a)))
      @(TTable context)
      @TColumns
      id
      id
      toColumns .
    from .
    Record

  default fromColumns ::
    ( Generic (Record a)
    , KnownAlgebra (GAlgebra (Rep (Record a)))
    , Eval (GGTable (GAlgebra (Rep (Record a))) (TTable context) TColumns (Col context) (Rep (Record a)))
    , Columns a ~ Eval (GGColumns (GAlgebra (Rep (Record a))) TColumns (Rep (Record a)))
    , Context a ~ Eval (GGContext (GAlgebra (Rep (Record a))) TContext (Rep (Record a)))
    )
    => Columns a (Col context) -> a
  fromColumns =
    unrecord .
    to .
    ggfromColumns
      @(GAlgebra (Rep (Record a)))
      @(TTable context)
      @TColumns
      id
      id
      fromColumns

  default reify ::
    ( Generic (Record a)
    , Generic (Record (Unreify a))
    , GMappable (TTable context) (Rep (Record a))
    , Rep (Record (Unreify a)) ~ GMap TUnreify (Rep (Record a))
    )
    => context :~: Reify ctx -> Unreify a -> a
  reify Refl =
    unrecord .
    to .
    gunmap @(TTable context) (Proxy @TUnreify) (reify Refl) .
    from .
    Record

  default unreify ::
    ( Generic (Record a)
    , Generic (Record (Unreify a))
    , GMappable (TTable context) (Rep (Record a))
    , Rep (Record (Unreify a)) ~ GMap TUnreify (Rep (Record a))
    )
    => context :~: Reify ctx -> a -> Unreify a
  unreify Refl =
    unrecord .
    to .
    gmap @(TTable context) (Proxy @TUnreify) (unreify Refl) .
    from .
    Record

  default coherence ::
    ( KnownAlgebra (GAlgebra (Rep (Record a)))
    , GCoherent (TTable context) (Rep (Record a))
    , Context a ~ Eval (GGContext (GAlgebra (Rep (Record a))) TContext (Rep (Record a)))
    , Context (Unreify a) ~ Eval (GGContext (GAlgebra (Rep (Record a))) TContext (Rep (Record (Unreify a))))
    , Rep (Record (Unreify a)) ~ GMap TUnreify (Rep (Record a))
    )
    => context :~: Reify ctx -> NotResult ctx -> Context (Unreify a) :~: ctx
  coherence = case algebraSing @(GAlgebra (Rep (Record a))) of
    SSum -> notReify
    SProduct -> \proof abstract ->
      gcoherence
        @(TTable context)
        @(Rep (Record a))
        (Proxy @TContext)
        (Proxy @TUnreify)
        (\(_ :: _proxy x) -> coherence @context @x proof abstract)

  default congruence ::
    ( KnownAlgebra (GAlgebra (Rep (Record a)))
    , GCoherent (TTable context) (Rep (Record a))
    , Context a ~ Eval (GGContext (GAlgebra (Rep (Record a))) TContext (Rep (Record a)))
    , Columns a ~ Eval (GGColumns (GAlgebra (Rep (Record a))) TColumns (Rep (Record a)))
    , Columns (Unreify a) ~ Eval (GGColumns (GAlgebra (Rep (Record a))) TColumns (Rep (Record (Unreify a))))
    , Rep (Record (Unreify a)) ~ GMap TUnreify (Rep (Record a))
    )
    => context :~: Reify ctx -> NotResult ctx -> Columns a :~: Columns (Unreify a)
  congruence = case algebraSing @(GAlgebra (Rep (Record a))) of
    SSum -> notReify
    SProduct -> \proof abstract ->
      gcongruence
        @(TTable context)
        @(Rep (Record a))
        (Proxy @TColumns)
        (Proxy @TUnreify)
        (\(_ :: _proxy x) -> congruence @context @x proof abstract)


data TTable :: K.Context -> Type -> Exp Constraint
type instance Eval (TTable context a) = Table context a


data TColumns :: Type -> Exp K.HTable
type instance Eval (TColumns a) = Columns a


data TContext :: Type -> Exp K.Context
type instance Eval (TContext a) = Context a


data TUnreify :: Type -> Exp Type
type instance Eval (TUnreify a) = Unreify a


type DefaultUnreify :: Type -> Type
type family DefaultUnreify a where
  DefaultUnreify (t a b c d e f g) =
    t (Unreify a) (Unreify b) (Unreify c) (Unreify d) (Unreify e) (Unreify f) (Unreify g)
  DefaultUnreify (t a b c d e f) =
    t (Unreify a) (Unreify b) (Unreify c) (Unreify d) (Unreify e) (Unreify f)
  DefaultUnreify (t a b c d e) =
    t (Unreify a) (Unreify b) (Unreify c) (Unreify d) (Unreify e)
  DefaultUnreify (t a b c d) =
    t (Unreify a) (Unreify b) (Unreify c) (Unreify d)
  DefaultUnreify (t a b c) = t (Unreify a) (Unreify b) (Unreify c)
  DefaultUnreify (t a b) = t (Unreify a) (Unreify b)
  DefaultUnreify (t a) = t (Unreify a)
  DefaultUnreify a = ARep (GMap TUnreify (Rep a))


-- | Any 'HTable' is also a 'Table'.
instance HTable t => Table context (t (Col context)) where
  type Columns (t (Col context)) = t
  type Context (t (Col context)) = context
  type Unreify (t (Col context)) = t (Col (UnwrapReify context))

  toColumns = id
  fromColumns = id

  reify Refl = hreify
  unreify Refl = hunreify
  coherence Refl _ = Refl
  congruence Refl _ = Refl


-- | Any context is trivially a table.
instance KnownSpec spec => Table context (Col context spec) where
  type Columns (Col context spec) = HIdentity spec
  type Context (Col context spec) = context
  type Unreify (Col context spec) = Col (UnwrapReify context) spec

  toColumns = HIdentity
  fromColumns = unHIdentity

  reify Refl = Reify
  unreify Refl (Reify a) = a
  coherence Refl _ = Refl
  congruence Refl _ = Refl


instance Sql DBType a => Table Result (Identity a) where
  type Columns (Identity a) = HType a
  type Context (Identity a) = Result

  toColumns (Identity a) = HType (R a)
  fromColumns (HType (R a)) = Identity a

  reify = notReify
  unreify = notReify
  coherence = notReify
  congruence = notReify


instance (context ~ Result, Table context a, Table context b) =>
  Table context (Either a b)
 where
  type Columns (Either a b) = HEitherTable (Columns a) (Columns b)
  type Context (Either a b) = Context a

  toColumns = \case
    Left table -> HEitherTable
      { htag = hlabel (HType (R IsLeft))
      , hleft = hlabel (hnullify nullifier (toColumns table))
      , hright = hlabel (hnulls (const null))
      }
    Right table -> HEitherTable
      { htag = hlabel (HType (R IsRight))
      , hleft = hlabel (hnulls (const null))
      , hright = hlabel (hnullify nullifier (toColumns table))
      }

  fromColumns HEitherTable {htag, hleft, hright} = case hunlabel htag of
    HType (R tag) -> case tag of
      IsLeft -> maybe err (Left . fromColumns) $ hunnullify unnullifier (hunlabel hleft)
      IsRight -> maybe err (Right . fromColumns) $ hunnullify unnullifier (hunlabel hright)
    where
      err = error "Either.fromColumns: mismatch between tag and data"

  coherence = notReify
  congruence = notReify


instance (context ~ Result, Table context a) => Table context [a] where
  type Columns [a] = HListTable (Columns a)
  type Context [a] = Context a

  toColumns = hvectorize vectorizer . fmap toColumns
  fromColumns = fmap fromColumns . hunvectorize unvectorizer

  coherence = notReify
  congruence = notReify


instance (context ~ Result, Table context a) => Table context (Maybe a) where
  type Columns (Maybe a) = HMaybeTable (Columns a)
  type Context (Maybe a) = Context a

  toColumns = \case
    Nothing -> HMaybeTable
      { htag = hlabel (HIdentity (R Nothing))
      , hjust = hlabel (hnulls (const null))
      }
    Just table -> HMaybeTable
      { htag = hlabel (HIdentity (R (Just IsJust)))
      , hjust = hlabel (hnullify nullifier (toColumns table))
      }

  fromColumns HMaybeTable {htag, hjust} = case hunlabel htag of
    HType (R tag) -> tag $>
      case hunnullify unnullifier (hunlabel hjust) of
        Nothing -> error "Maybe.fromColumns: mismatch between tag and data"
        Just just -> fromColumns just

  coherence = notReify
  congruence = notReify


instance (context ~ Result, Table context a) => Table context (NonEmpty a)
 where
  type Columns (NonEmpty a) = HNonEmptyTable (Columns a)
  type Context (NonEmpty a) = Context a

  toColumns = hvectorize vectorizer . fmap toColumns
  fromColumns = fmap fromColumns . hunvectorize unvectorizer

  coherence = notReify
  congruence = notReify


instance (context ~ Result, Table context a, Table context b) =>
  Table context (These a b)
 where
  type Columns (These a b) = HTheseTable (Columns a) (Columns b)
  type Context (These a b) = Context a

  toColumns tables = HTheseTable
    { hhereTag = hrelabel hhereTag
    , hhere = hrelabel (toColumns hhere)
    , hthereTag = hrelabel hthereTag
    , hthere = hrelabel (toColumns hthere)
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
        { htag = hrelabel hhereTag
        , hjust = hrelabel hhere
        }
      mthere = HMaybeTable
        { htag = hrelabel hthereTag
        , hjust = hrelabel hthere
        }

  coherence = notReify
  congruence = notReify


instance (Table context a, Table context b) => Table context (a, b)


instance
  ( Table context a, Table context b, Table context c
  )
  => Table context (a, b, c)


instance
  ( Table context a, Table context b, Table context c, Table context d
  )
  => Table context (a, b, c, d)


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e
  )
  => Table context (a, b, c, d, e)


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e, Table context f
  )
  => Table context (a, b, c, d, e, f)


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e, Table context f, Table context g
  )
  => Table context (a, b, c, d, e, f, g)


type Congruent :: Type -> Type -> Constraint
class Columns a ~ Columns b => Congruent a b
instance Columns a ~ Columns b => Congruent a b
