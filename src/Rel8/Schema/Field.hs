{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Field
  ( Field
  , HEither, HList, HMaybe, HNonEmpty, HThese
  , Reify, hreify, hunreify
  , Reifiable(..)
  , AField(..)
  , AHEither(..), AHList(..), AHMaybe(..), AHNonEmpty(..), AHThese(..)
  , SContext(..)
  , Col( Reify )
  )
where

-- base
import Control.Applicative ( Const( Const ), getConst )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Compose ( Compose( Compose ), getCompose )
import Data.Functor.Identity ( Identity, runIdentity )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Col(..) )
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Kind.Necessity
  ( Necessity( Required, Optional )
  , SNecessity( SRequired, SOptional )
  , KnownNecessity, necessitySing
  )
import Rel8.Schema.Context ( Interpretation, Col(..) )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, hfield, hmap, htabulate )
import Rel8.Schema.HTable.Either ( HEitherTable )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Maybe ( HMaybeTable )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.These ( HTheseTable )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Insert ( Insert, Col(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..), Col(..) )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Congruent
  )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.List ( ListTable( ListTable ) )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.These ( TheseTable )
import Rel8.Type ( DBType )

-- these
import Data.These ( These )


type Field :: K.Context -> Necessity -> Type -> Type
type family Field context necessity a where
  Field (Reify context) necessity  a = AField context necessity a
  Field Identity        _necessity a = a
  Field Expr            _necessity a = Expr a
  Field Insert          'Required  a = Expr a
  Field Insert          'Optional  a = Maybe (Expr a)
  Field Aggregate       _necessity a = Aggregate (Expr a)
  Field context         _necessity a = context a


type HEither :: K.Context -> Type -> Type -> Type
type family HEither context where
  HEither (Reify context) = AHEither context
  HEither Aggregate = EitherTable
  HEither Expr = EitherTable
  HEither Identity = Either
  HEither Insert = EitherTable
  HEither Name = EitherTable
  HEither _ = Either


type HList :: K.Context -> Type -> Type
type family HList context where
  HList (Reify context) = AHList context
  HList Aggregate = ListTable
  HList Expr = ListTable
  HList Identity = []
  HList Insert = ListTable
  HList Name = ListTable
  HList _ = []


type HMaybe :: K.Context -> Type -> Type
type family HMaybe context where
  HMaybe (Reify context) = AHMaybe context
  HMaybe Aggregate = MaybeTable
  HMaybe Expr = MaybeTable
  HMaybe Identity = Maybe
  HMaybe Insert = MaybeTable
  HMaybe Name = MaybeTable
  HMaybe _ = Maybe


type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context where
  HNonEmpty (Reify context) = AHNonEmpty context
  HNonEmpty Aggregate = NonEmptyTable
  HNonEmpty Expr = NonEmptyTable
  HNonEmpty Identity = NonEmpty
  HNonEmpty Insert = NonEmptyTable
  HNonEmpty Name = NonEmptyTable
  HNonEmpty _ = NonEmpty


type HThese :: K.Context -> Type -> Type -> Type
type family HThese context where
  HThese (Reify context) = AHThese context
  HThese Aggregate = TheseTable
  HThese Expr = TheseTable
  HThese Identity = These
  HThese Insert = TheseTable
  HThese Name = TheseTable
  HThese _ = These


type AField :: K.Context -> Necessity -> Type -> Type
newtype AField context necessity a = AField (Field context necessity a)


instance (Reifiable context, KnownNecessity necessity, Sql DBType a) =>
  Table (Reify context) (AField context necessity a)
 where
  type Context (AField context necessity a) = Reify context
  type Columns (AField context necessity a) = HIdentity ('Spec '[""] necessity a)

  fromColumns (HIdentity (Reify a)) = sfromColumn contextSing a
  toColumns = HIdentity . Reify . stoColumn contextSing necessitySing


instance
  ( Reifiable context, Reifiable context'
  , KnownNecessity necessity, Sql DBType a
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AField context necessity a)
    (AField context' necessity a)


type AHEither :: K.Context -> Type -> Type -> Type
newtype AHEither context a b = AHEither (HEither context a b)


instance Reifiable context => Bifunctor (AHEither context) where
  bimap = sbimapEither contextSing


instance Reifiable context => Functor (AHEither context a) where
  fmap = bimap id


instance (Reifiable context, Table (Reify context) a, Table (Reify context) b)
  => Table (Reify context) (AHEither context a b)
 where
  type Context (AHEither context a b) = Reify context
  type Columns (AHEither context a b) = HEitherTable (Columns a) (Columns b)

  fromColumns = sfromColumnsEither contextSing
  toColumns = stoColumnsEither contextSing


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  , Recontextualize (Reify context) (Reify context') b b'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHEither context a b)
    (AHEither context' a' b')


type AHList :: K.Context -> Type -> Type
newtype AHList context a = AHList (HList context a)


instance (Reifiable context, Table (Reify context) a) =>
  Table (Reify context) (AHList context a)
 where
  type Context (AHList context a) = Reify context
  type Columns (AHList context a) = HListTable (Columns a)

  fromColumns = sfromColumnsList contextSing
  toColumns = stoColumnsList contextSing


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHList context a)
    (AHList context' a')


type AHMaybe :: K.Context -> Type -> Type
newtype AHMaybe context a = AHMaybe (HMaybe context a)


instance Reifiable context => Functor (AHMaybe context) where
  fmap = smapMaybe contextSing


instance (Reifiable context, Table (Reify context) a) =>
  Table (Reify context) (AHMaybe context a)
 where
  type Context (AHMaybe context a) = Reify context
  type Columns (AHMaybe context a) = HMaybeTable (Columns a)

  fromColumns = sfromColumnsMaybe contextSing
  toColumns = stoColumnsMaybe contextSing


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHMaybe context a)
    (AHMaybe context' a')


type AHNonEmpty :: K.Context -> Type -> Type
newtype AHNonEmpty context a = AHNonEmpty (HNonEmpty context a)


instance (Reifiable context, Table (Reify context) a) =>
  Table (Reify context) (AHNonEmpty context a)
 where
  type Context (AHNonEmpty context a) = Reify context
  type Columns (AHNonEmpty context a) = HNonEmptyTable (Columns a)

  fromColumns = sfromColumnsNonEmpty contextSing
  toColumns = stoColumnsNonEmpty contextSing


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHNonEmpty context a)
    (AHNonEmpty context' a')


type AHThese :: K.Context -> Type -> Type -> Type
newtype AHThese context a b = AHThese (HThese context a b)


instance Reifiable context => Bifunctor (AHThese context) where
  bimap = sbimapThese contextSing


instance Reifiable context => Functor (AHThese context a) where
  fmap = bimap id


instance (Reifiable context, Table (Reify context) a, Table (Reify context) b)
  => Table (Reify context) (AHThese context a b)
 where
  type Context (AHThese context a b) = Reify context
  type Columns (AHThese context a b) = HTheseTable (Columns a) (Columns b)

  fromColumns = sfromColumnsThese contextSing
  toColumns = stoColumnsThese contextSing


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  , Recontextualize (Reify context) (Reify context') b b'
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (AHThese context a b)
    (AHThese context' a' b')


type SContext :: K.Context -> Type
data SContext context where
  SAggregate :: SContext Aggregate
  SExpr :: SContext Expr
  SIdentity :: SContext Identity
  SInsert :: SContext Insert
  SName :: SContext Name
  SReify :: SContext context -> SContext (Reify context)


type Reifiable :: K.Context -> Constraint
class Interpretation context => Reifiable context where
  contextSing :: SContext context


instance Reifiable Aggregate where
  contextSing = SAggregate


instance Reifiable Expr where
  contextSing = SExpr


instance Reifiable Identity where
  contextSing = SIdentity


instance Reifiable Insert where
  contextSing = SInsert


instance Reifiable Name where
  contextSing = SName


type Reify :: K.Context -> K.Context
data Reify context a


instance Interpretation (Reify context) where
  newtype Col (Reify context) spec = Reify (Col context spec)


instance Labelable context => Labelable (Reify context) where
  labeler (Reify a) = Reify (labeler a)
  unlabeler (Reify a) = Reify (unlabeler a)


instance Reifiable context => Reifiable (Reify context) where
  contextSing = SReify contextSing


sfromColumn :: ()
  => SContext context
  -> Col context ('Spec labels necessity a)
  -> AField context necessity a
sfromColumn = \case
  SAggregate -> \(Aggregation a) -> AField a
  SExpr -> \(DB a) -> AField a
  SIdentity -> \(Result a) -> AField a
  SInsert -> \case
    RequiredInsert a -> AField a
    OptionalInsert a -> AField a
  SName -> \(NameCol a) -> AField (Name a)
  SReify context -> \(Reify a) -> AField (sfromColumn context a)


stoColumn :: ()
  => SContext context
  -> SNecessity necessity
  -> AField context necessity a
  -> Col context ('Spec labels necessity a)
stoColumn = \case
  SAggregate -> \_ (AField a) -> Aggregation a
  SExpr -> \_ (AField a) -> DB a
  SIdentity -> \_ (AField a) -> Result a
  SInsert -> \case
    SRequired -> \(AField a) -> RequiredInsert a
    SOptional -> \(AField a) -> OptionalInsert a
  SName -> \_ (AField (Name a)) -> NameCol a
  SReify context ->
    \necessity (AField a) -> Reify (stoColumn context necessity a)


sbimapEither :: ()
  => SContext context
  -> (a -> c)
  -> (b -> d)
  -> AHEither context a b
  -> AHEither context c d
sbimapEither = \case
  SAggregate -> \f g (AHEither a) -> AHEither (bimap f g a)
  SExpr -> \f g (AHEither a) -> AHEither (bimap f g a)
  SIdentity -> \f g (AHEither a) -> AHEither (bimap f g a)
  SInsert -> \f g (AHEither a) -> AHEither (bimap f g a)
  SName -> \f g (AHEither a) -> AHEither (bimap f g a)
  SReify context -> \f g (AHEither a) -> AHEither (sbimapEither context f g a)


sfromColumnsEither :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> HEitherTable (Columns a) (Columns b) (Col (Reify context))
  -> AHEither context a b
sfromColumnsEither = \case
  SAggregate ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SExpr ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SIdentity ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SInsert ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SName ->
    AHEither .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SReify context ->
    AHEither .
    sbimapEither context (fromColumns . hreify) (fromColumns . hreify) .
    sfromColumnsEither context .
    hunreify


stoColumnsEither :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> AHEither context a b
  -> HEitherTable (Columns a) (Columns b) (Col (Reify context))
stoColumnsEither = \case
  SAggregate ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SExpr ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SIdentity ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SInsert ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SName ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)
  SReify context ->
    hreify .
    stoColumnsEither context .
    sbimapEither context (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHEither a) -> a)


smapList :: Congruent a b
  => SContext context
  -> (a -> b)
  -> (HListTable (Columns a) (Col (Context a)) -> HListTable (Columns b) (Col (Context b)))
  -> AHList context a
  -> AHList context b
smapList = \case
  SAggregate -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SExpr -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SIdentity -> \f _ (AHList as) -> AHList (fmap f as)
  SInsert -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SName -> \_ f (AHList (ListTable a)) -> AHList (ListTable (f a))
  SReify context -> \f g (AHList as) -> AHList (smapList context f g as)


sfromColumnsList :: Table (Reify context) a
  => SContext context
  -> HListTable (Columns a) (Col (Reify context))
  -> AHList context a
sfromColumnsList = \case
  SAggregate -> AHList . ListTable
  SExpr -> AHList . ListTable
  SIdentity -> AHList . fmap (fromColumns . hreify) . fromColumns . hunreify
  SInsert -> AHList . ListTable
  SName -> AHList . ListTable
  SReify context ->
    AHList .
    smapList context (fromColumns . hreify) hreify .
    sfromColumnsList context .
    hunreify


stoColumnsList :: Table (Reify context) a
  => SContext context
  -> AHList context a
  -> HListTable (Columns a) (Col (Reify context))
stoColumnsList = \case
  SAggregate -> \(AHList (ListTable a)) -> a
  SExpr -> \(AHList (ListTable a)) -> a
  SIdentity ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHList a) -> a)
  SInsert -> \(AHList (ListTable a)) -> a
  SName -> \(AHList (ListTable a)) -> a
  SReify context ->
    hreify .
    stoColumnsList context .
    smapList context (hunreify . toColumns) hunreify .
    (\(AHList a) -> a)


smapMaybe :: ()
  => SContext context
  -> (a -> b)
  -> AHMaybe context a
  -> AHMaybe context b
smapMaybe = \case
  SAggregate -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SExpr -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SIdentity -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SInsert -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SName -> \f (AHMaybe a) -> AHMaybe (fmap f a)
  SReify context -> \f (AHMaybe a) -> AHMaybe (smapMaybe context f a)


sfromColumnsMaybe :: Table (Reify context) a
  => SContext context
  -> HMaybeTable (Columns a) (Col (Reify context))
  -> AHMaybe context a
sfromColumnsMaybe = \case
  SAggregate -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SExpr -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SIdentity -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SInsert -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SName -> AHMaybe . fmap (fromColumns . hreify) . fromColumns . hunreify
  SReify context ->
    AHMaybe .
    smapMaybe context (fromColumns . hreify) .
    sfromColumnsMaybe context .
    hunreify


stoColumnsMaybe :: Table (Reify context) a
  => SContext context
  -> AHMaybe context a
  -> HMaybeTable (Columns a) (Col (Reify context))
stoColumnsMaybe = \case
  SAggregate ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SExpr ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SIdentity ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SInsert ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SName ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHMaybe a) -> a)
  SReify context ->
    hreify .
    stoColumnsMaybe context .
    smapMaybe context (hunreify . toColumns) .
    (\(AHMaybe a) -> a)


smapNonEmpty :: Congruent a b
  => SContext context
  -> (a -> b)
  -> (HNonEmptyTable (Columns a) (Col (Context a)) -> HNonEmptyTable (Columns b) (Col (Context b)))
  -> AHNonEmpty context a
  -> AHNonEmpty context b
smapNonEmpty = \case
  SAggregate -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
  SExpr -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
  SIdentity -> \f _ (AHNonEmpty as) -> AHNonEmpty (fmap f as)
  SInsert -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
  SName -> \_ f (AHNonEmpty (NonEmptyTable a)) -> AHNonEmpty (NonEmptyTable (f a))
  SReify context -> \f g (AHNonEmpty as) -> AHNonEmpty (smapNonEmpty context f g as)


sfromColumnsNonEmpty :: Table (Reify context) a
  => SContext context
  -> HNonEmptyTable (Columns a) (Col (Reify context))
  -> AHNonEmpty context a
sfromColumnsNonEmpty = \case
  SAggregate -> AHNonEmpty . NonEmptyTable
  SExpr -> AHNonEmpty . NonEmptyTable
  SIdentity ->
    AHNonEmpty . fmap (fromColumns . hreify) . fromColumns . hunreify
  SInsert -> AHNonEmpty . NonEmptyTable
  SName -> AHNonEmpty . NonEmptyTable
  SReify context ->
    AHNonEmpty .
    smapNonEmpty context (fromColumns . hreify) hreify .
    sfromColumnsNonEmpty context .
    hunreify


stoColumnsNonEmpty :: Table (Reify context) a
  => SContext context
  -> AHNonEmpty context a
  -> HNonEmptyTable (Columns a) (Col (Reify context))
stoColumnsNonEmpty = \case
  SAggregate -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SExpr -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SIdentity ->
    hreify . toColumns . fmap (hunreify . toColumns) . (\(AHNonEmpty a) -> a)
  SInsert -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SName -> \(AHNonEmpty (NonEmptyTable a)) -> a
  SReify context ->
    hreify .
    stoColumnsNonEmpty context .
    smapNonEmpty context (hunreify . toColumns) hunreify .
    (\(AHNonEmpty a) -> a)


sbimapThese :: ()
  => SContext context
  -> (a -> c)
  -> (b -> d)
  -> AHThese context a b
  -> AHThese context c d
sbimapThese = \case
  SAggregate -> \f g (AHThese a) -> AHThese (bimap f g a)
  SExpr -> \f g (AHThese a) -> AHThese (bimap f g a)
  SIdentity -> \f g (AHThese a) -> AHThese (bimap f g a)
  SInsert -> \f g (AHThese a) -> AHThese (bimap f g a)
  SName -> \f g (AHThese a) -> AHThese (bimap f g a)
  SReify context -> \f g (AHThese a) -> AHThese (sbimapThese context f g a)


sfromColumnsThese :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> HTheseTable (Columns a) (Columns b) (Col (Reify context))
  -> AHThese context a b
sfromColumnsThese = \case
  SAggregate ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SExpr ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SIdentity ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SInsert ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SName ->
    AHThese .
    bimap (fromColumns . hreify) (fromColumns . hreify) .
    fromColumns .
    hunreify
  SReify context ->
    AHThese .
    sbimapThese context (fromColumns . hreify) (fromColumns . hreify) .
    sfromColumnsThese context .
    hunreify


stoColumnsThese :: (Table (Reify context) a, Table (Reify context) b)
  => SContext context
  -> AHThese context a b
  -> HTheseTable (Columns a) (Columns b) (Col (Reify context))
stoColumnsThese = \case
  SAggregate ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SExpr ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SIdentity ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SInsert ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SName ->
    hreify .
    toColumns .
    bimap (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)
  SReify context ->
    hreify .
    stoColumnsThese context .
    sbimapThese context (hunreify . toColumns) (hunreify . toColumns) .
    (\(AHThese a) -> a)


hreify :: HTable t => t (Col context) -> t (Col (Reify context))
hreify a = htabulate $ \field -> Reify (hfield a field)


hunreify :: HTable t => t (Col (Reify context)) -> t (Col context)
hunreify a = htabulate $ \field -> case hfield a field of
  Reify x -> x
