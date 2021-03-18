{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language DefaultSignatures #-}
{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Schema.Generic
  ( Rel8able
  )
where

-- base
import Data.Bifunctor ( bimap )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics
  ( Generic, Rep, from, to
  , (:*:)( (:*:) ), K1( K1 ), M1( M1 )
  , D, C, S
  , Meta( MetaSel )
  )
import GHC.TypeLits ( Symbol )
import Prelude

-- rel8
import Rel8.Kind.Necessity
  ( SNecessity( SRequired, SOptional )
  , KnownNecessity, necessitySing
  )
import Rel8.Kind.Nullability
  ( SNullability( SNonNullable, SNullable )
  , KnownNullability, nullabilitySing
  )
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB )
  , Insert( RequiredInsert, OptionalInsert )
  , Result( Result )
  , IsSpecialContext
  )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.Context.Nullify ( Nullifiable )
import Rel8.Schema.Context.Result
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
import Rel8.Schema.Field ( Field )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.HTable.Either ( HEitherTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Maybe ( HMaybeTable )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.These ( HTheseTable )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Vectorize ( hrelabel )
import Rel8.Schema.Spec ( Spec( Spec ), KTable )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Schema.Structure
  ( IsStructure, Shape(..), Shape1, Shape2
  , Structure
  )
import Rel8.Schema.Value ( Value( NullableValue, NonNullableValue ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.Lifted
  ( fromColumns1, toColumns1
  , fromColumns2, toColumns2
  )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( MaybeTable(..) )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.These ( TheseTable )

-- these
import Data.These ( These )


instance (Rel8able t, TableHelper (IsSpecialContext context) context) =>
  Table context (t context)
 where
  type Columns (t context) = GRep t
  type Context (t context) = context
  fromColumns = gfromColumns
  toColumns = gtoColumns


type TableHelper :: Bool -> Kind.Context -> Constraint
class IsSpecialContext context ~ isSpecialContext =>
  TableHelper isSpecialContext context
 where
   gfromColumns :: Rel8able t => GRep t (H context) -> t context
   gtoColumns :: Rel8able t => t context -> GRep t (H context)


instance TableHelper 'True Aggregation where
  gfromColumns = fromAggregationColumns
  gtoColumns = toAggregationColumns


instance TableHelper 'True DB where
  gfromColumns = fromDBColumns
  gtoColumns = toDBColumns


instance TableHelper 'True Insert where
  gfromColumns = fromInsertColumns
  gtoColumns = toInsertColumns


instance TableHelper 'True Result where
  gfromColumns = fromResultColumns
  gtoColumns = toResultColumns


instance (IsSpecialContext context ~ 'False, Labelable context, Nullifiable context) =>
  TableHelper 'False context
 where
  gfromColumns = fromUnspecialColumns
  gtoColumns = toUnspecialColumns


type Rel8able :: KTable -> Constraint
class HTable (GRep t) => Rel8able t where
  type GRep t :: HKTable

  fromAggregationColumns :: GRep t (H Aggregation) -> t Aggregation
  toAggregationColumns :: t Aggregation -> GRep t (H Aggregation)

  fromDBColumns :: GRep t (H DB) -> t DB
  toDBColumns :: t DB -> GRep t (H DB)

  fromInsertColumns :: GRep t (H Insert) -> t Insert
  toInsertColumns :: t Insert -> GRep t (H Insert)

  fromResultColumns :: GRep t (H Result) -> t Result
  toResultColumns :: t Result -> GRep t (H Result)

  fromUnspecialColumns ::
    ( IsSpecialContext context ~ 'False
    , Labelable context, Nullifiable context
    )
    => GRep t (H context) -> t context
  toUnspecialColumns ::
    ( IsSpecialContext context ~ 'False
    , Labelable context, Nullifiable context
    )
    => t context -> GRep t (H context)

  type GRep t = GColumns (Rep (t Structure))

  default fromAggregationColumns ::
    ( Generic (t Aggregation)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Aggregation (Rep (t Structure)) (Rep (t Aggregation))
    ) => GRep t (H Aggregation) -> t Aggregation
  fromAggregationColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toAggregationColumns ::
    ( Generic (t Aggregation)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Aggregation (Rep (t Structure)) (Rep (t Aggregation))
    ) => t Aggregation -> GRep t (H Aggregation)
  toAggregationColumns = toGColumns @_ @(Rep (t Structure)) . from

  default fromDBColumns ::
    ( Generic (t DB)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able DB (Rep (t Structure)) (Rep (t DB))
    ) => GRep t (H DB) -> t DB
  fromDBColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toDBColumns ::
    ( Generic (t DB)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able DB (Rep (t Structure)) (Rep (t DB))
    ) => t DB -> GRep t (H DB)
  toDBColumns = toGColumns @_ @(Rep (t Structure)) . from

  default fromInsertColumns ::
    ( Generic (t Insert)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Insert (Rep (t Structure)) (Rep (t Insert))
    ) => GRep t (H Insert) -> t Insert
  fromInsertColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toInsertColumns ::
    ( Generic (t Insert)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Insert (Rep (t Structure)) (Rep (t Insert))
    ) => t Insert -> GRep t (H Insert)
  toInsertColumns = toGColumns @_ @(Rep (t Structure)) . from

  default fromResultColumns ::
    ( Generic (t Result)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Result (Rep (t Structure)) (Rep (t Result))
    ) => GRep t (H Result) -> t Result
  fromResultColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toResultColumns ::
    ( Generic (t Result)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Result (Rep (t Structure)) (Rep (t Result))
    ) => t Result -> GRep t (H Result)
  toResultColumns = toGColumns @_ @(Rep (t Structure)) . from

  default fromUnspecialColumns ::
    ( Generic (t context)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able context (Rep (t Structure)) (Rep (t context))
    ) => GRep t (H context) -> t context
  fromUnspecialColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toUnspecialColumns ::
    ( Generic (t context)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able context (Rep (t Structure)) (Rep (t context))
    ) => t context -> GRep t (H context)
  toUnspecialColumns = toGColumns @_ @(Rep (t Structure)) . from


type GColumns :: (Type -> Type) -> HKTable
type family GColumns structure where
  GColumns (M1 D _ structure) = GColumns structure
  GColumns (M1 C _ structure) = GColumns structure
  GColumns (a :*: b) = HPair (GColumns a) (GColumns b)
  GColumns (M1 S ('MetaSel ('Just label) _ _ _) (K1 _ structure)) =
    K1Columns label structure


type GRel8able :: Kind.Context -> (Type -> Type) -> (Type -> Type) -> Constraint
class GRel8able context structure rep where
  fromGColumns :: GColumns structure (H context) -> rep x
  toGColumns :: rep x -> GColumns structure (H context)


instance GRel8able context structure rep => GRel8able context (M1 D c structure) (M1 D c rep) where
  fromGColumns = M1 . fromGColumns @context @structure @rep
  toGColumns (M1 a) = toGColumns @context @structure @rep a


instance GRel8able context structure rep => GRel8able context (M1 C c structure) (M1 C c rep) where
  fromGColumns = M1 . fromGColumns @context @structure @rep
  toGColumns (M1 a) = toGColumns @context @structure @rep a


instance (GRel8able context structure1 rep1, GRel8able context structure2 rep2) =>
  GRel8able context (structure1 :*: structure2) (rep1 :*: rep2)
 where
  fromGColumns (HPair a b) =
    fromGColumns @context @structure1 @rep1 a :*:
    fromGColumns @context @structure2 @rep2 b
  toGColumns (a :*: b) =
    HPair
      (toGColumns @context @structure1 @rep1 a)
      (toGColumns @context @structure2 @rep2 b)


instance
  ( K1Table label isSpecialContext context isStructure structure a
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , structureK1 ~ K1 i structure
  , k1 ~ K1 i a
  ) => GRel8able context (M1 S meta structureK1) (M1 S meta k1)
 where
  fromGColumns = M1 . K1 . fromK1Columns @label @_ @_ @_ @structure
  toGColumns (M1 (K1 a)) = toK1Columns @label @_ @_ @_ @structure a


type K1Columns :: Symbol -> Type -> HKTable
type family K1Columns label structure where
  K1Columns label (Shape1 'Column ('Spec '[] necessity nullability blueprint)) =
    HIdentity ('Spec '[label] necessity nullability blueprint)
  K1Columns _label (Shape1 'Column ('Spec (label ': labels) necessity nullability blueprint)) =
    HIdentity ('Spec (label ': labels) necessity nullability blueprint)
  K1Columns label (Shape2 'Either a b) =
    HLabel label (HEitherTable (K1Columns "Left" a) (K1Columns "Right" b))
  K1Columns label (Shape1 'List a) = HListTable (K1Columns label a)
  K1Columns label (Shape1 'Maybe a) = HLabel label (HMaybeTable (K1Columns "Just" a))
  K1Columns label (Shape1 'NonEmpty a) = HNonEmptyTable (K1Columns label a)
  K1Columns label (Shape2 'These a b) =
    HLabel label (HTheseTable (K1Columns "Here" a) (K1Columns "There" b))
  K1Columns label (a, b) =
    HLabel label
      (HPair
        (K1Columns "fst" a)
        (K1Columns "snd" b))
  K1Columns label (a, b, c) =
    HLabel label
      (HTrio
        (K1Columns "fst" a)
        (K1Columns "snd" b)
        (K1Columns "trd" c))
  K1Columns label (a, b, c, d) =
    HLabel label
      (HQuartet
        (K1Columns "fst" a)
        (K1Columns "snd" b)
        (K1Columns "trd" c)
        (K1Columns "frt" d))
  K1Columns label (a, b, c, d, e) =
    HLabel label
      (HQuintet
        (K1Columns "fst" a)
        (K1Columns "snd" b)
        (K1Columns "trd" c)
        (K1Columns "frt" d)
        (K1Columns "fft" e))
  K1Columns label a = HLabel label (Columns a)


type K1Table :: Symbol -> Bool -> Kind.Context -> Bool -> Type -> Type -> Constraint
class
  ( isSpecialContext ~ IsSpecialContext context
  , isStructure ~ IsStructure structure
  ) => K1Table label isSpecialContext context isStructure structure a
 where
  fromK1Columns :: K1Columns label structure (H context) -> a
  toK1Columns :: a -> K1Columns label structure (H context)


instance
  ( a ~ Field Aggregation '[] necessity nullability blueprint
  ) => K1Table label 'True Aggregation 'True (Shape1 'Column ('Spec '[] necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity (Aggregation a)) = a
  toK1Columns = HIdentity . Aggregation


instance
  ( a ~ Field Aggregation (label ': labels) necessity nullability blueprint
  ) => K1Table _label 'True Aggregation 'True (Shape1 'Column ('Spec (label ': labels) necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity (Aggregation a)) = a
  toK1Columns = HIdentity . Aggregation


instance
  ( a ~ Field DB '[] necessity nullability blueprint
  ) => K1Table label 'True DB 'True (Shape1 'Column ('Spec '[] necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity (DB a)) = a
  toK1Columns = HIdentity . DB


instance
  ( a ~ Field DB (label ': labels) necessity nullability blueprint
  ) => K1Table _label 'True DB 'True (Shape1 'Column ('Spec (label ': labels) necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity (DB a)) = a
  toK1Columns = HIdentity . DB


instance
  ( a ~ Field Insert '[] necessity nullability blueprint
  , KnownNecessity necessity
  ) => K1Table label 'True Insert 'True (Shape1 'Column ('Spec '[] necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity insert) = case insert of
    RequiredInsert a -> a
    OptionalInsert ma -> ma
  toK1Columns a = HIdentity $ case necessitySing @necessity of
    SRequired -> RequiredInsert a
    SOptional -> OptionalInsert a


instance
  ( a ~ Field Insert (label ': labels) necessity nullability blueprint
  , KnownNecessity necessity
  ) => K1Table _label 'True Insert 'True (Shape1 'Column ('Spec (label ': labels) necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity insert) = case insert of
    RequiredInsert a -> a
    OptionalInsert ma -> ma
  toK1Columns a = HIdentity $ case necessitySing @necessity of
    SRequired -> RequiredInsert a
    SOptional -> OptionalInsert a


instance
  ( a ~ Field Result '[] necessity nullability blueprint
  , KnownNullability nullability
  ) => K1Table label 'True Result 'True (Shape1 'Column ('Spec '[] necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity (Result value)) = case value of
    NullableValue ma -> ma
    NonNullableValue a -> a
  toK1Columns a = HIdentity $ Result $ case nullabilitySing @nullability of
    SNullable -> NullableValue a
    SNonNullable -> NonNullableValue a


instance
  ( a ~ Field Result (label ': labels) necessity nullability blueprint
  , KnownNullability nullability
  ) => K1Table _label 'True Result 'True (Shape1 'Column ('Spec (label ': labels) necessity nullability blueprint)) a
 where
  fromK1Columns (HIdentity (Result value)) = case value of
    NullableValue ma -> ma
    NonNullableValue a -> a
  toK1Columns a = HIdentity $ Result $ case nullabilitySing @nullability of
    SNullable -> NullableValue a
    SNonNullable -> NonNullableValue a


instance
  ( IsSpecialContext context ~ 'False
  , a ~ context ('Spec (label ': labels) necessity nullability blueprint)
  ) => K1Table _label 'False context 'True (Shape1 'Column ('Spec (label ': labels) necessity nullability blueprint)) a
 where
  fromK1Columns = unHIdentity
  toK1Columns = HIdentity


instance
  ( IsSpecialContext context ~ 'False
  , a ~ context ('Spec '[] necessity nullability blueprint)
  , Labelable context
  ) => K1Table label 'False context 'True (Shape1 'Column ('Spec '[] necessity nullability blueprint)) a
 where
  fromK1Columns = unlabeler . unHIdentity
  toK1Columns = HIdentity . labeler

instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" 'True Aggregation (IsStructure structure1) structure1 a
  , K1Table "Right" 'True Aggregation (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  ) => K1Table label 'True Aggregation 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Left" @_ @_ @_ @structure1)
      (fromK1Columns @"Right" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Left" @_ @_ @_ @structure1)
      (toK1Columns @"Right" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" 'True DB (IsStructure structure1) structure1 a
  , K1Table "Right" 'True DB (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  ) => K1Table label 'True DB 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Left" @_ @_ @_ @structure1)
      (fromK1Columns @"Right" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Left" @_ @_ @_ @structure1)
      (toK1Columns @"Right" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" 'True Insert (IsStructure structure1) structure1 a
  , K1Table "Right" 'True Insert (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  ) => K1Table label 'True Insert 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Left" @_ @_ @_ @structure1)
      (fromK1Columns @"Right" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Left" @_ @_ @_ @structure1)
      (toK1Columns @"Right" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" 'True Result (IsStructure structure1) structure1 a
  , K1Table "Right" 'True Result (IsStructure structure2) structure2 b
  , e ~ Either a b
  ) => K1Table label 'True Result 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns
    = bimap
        (fromK1Columns @"Left" @_ @_ @_ @structure1)
        (fromK1Columns @"Right" @_ @_ @_ @structure2)
    . fromHEitherTable
    . hunlabel unlabeler
  toK1Columns
    = hlabel labeler
    . toHEitherTable
    . bimap
        (toK1Columns @"Left" @_ @_ @_ @structure1)
        (toK1Columns @"Right" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" 'False context (IsStructure structure1) structure1 a
  , K1Table "Right" 'False context (IsStructure structure2) structure2 b
  , IsSpecialContext context ~ 'False
  , Labelable context
  , Nullifiable context
  , e ~ EitherTable a b
  ) => K1Table label 'False context 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Left" @_ @_ @_ @structure1)
      (fromK1Columns @"Right" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Left" @_ @_ @_ @structure1)
      (toK1Columns @"Right" @_ @_ @_ @structure2)


instance
  ( K1Table label 'True Aggregation (IsStructure structure) structure a
  , Table Aggregation a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ ListTable a
  ) => K1Table label 'True Aggregation 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( K1Table label 'True DB (IsStructure structure) structure a
  , Table DB a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ ListTable a
  ) => K1Table label 'True DB 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( K1Table label 'True Insert (IsStructure structure) structure a
  , Table Insert a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ ListTable a
  ) => K1Table label 'True Insert 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( K1Table label 'True Result (IsStructure structure) structure a
  , HTable (K1Columns label structure)
  , as ~ [a]
  ) => K1Table label 'True Result 'True (Shape1 'List structure) as
 where
  fromK1Columns = fmap (fromK1Columns @label @'True @Result @_ @structure) . fromHListTable
  toK1Columns = toHListTable . fmap (toK1Columns @label @'True @Result @_ @structure)


instance
  ( K1Table label 'False context (IsStructure structure) structure a
  , IsSpecialContext context ~ 'False
  , Table context a
  , K1Columns label structure ~ HLabel label (Columns a)
  , Labelable context
  , as ~ ListTable a
  ) => K1Table label 'False context 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" 'True Aggregation (IsStructure structure) structure a
  , ma ~ MaybeTable a
  ) => K1Table label 'True Aggregation 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns =
    fromColumns1 (fromK1Columns @"Just" @_ @_ @_ @structure) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns1 (toK1Columns @"Just" @_ @_ @_ @structure)


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" 'True DB (IsStructure structure) structure a
  , ma ~ MaybeTable a
  ) => K1Table label 'True DB 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns =
    fromColumns1 (fromK1Columns @"Just" @_ @_ @_ @structure) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns1 (toK1Columns @"Just" @_ @_ @_ @structure)


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" 'True Insert (IsStructure structure) structure a
  , ma ~ MaybeTable a
  ) => K1Table label 'True Insert 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns =
    fromColumns1 (fromK1Columns @"Just" @_ @_ @_ @structure) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns1 (toK1Columns @"Just" @_ @_ @_ @structure)


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" 'True Result (IsStructure structure) structure a
  , ma ~ Maybe a
  ) => K1Table label 'True Result 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns
    = fmap (fromK1Columns @"Just" @_ @_ @_ @structure)
    . fromHMaybeTable
    . hunlabel unlabeler
  toK1Columns
    = hlabel labeler
    . toHMaybeTable
    . fmap (toK1Columns @"Just" @_ @_ @_ @structure)


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" 'False context (IsStructure structure) structure a
  , IsSpecialContext context ~ 'False
  , Labelable context
  , Nullifiable context
  , ma ~ MaybeTable a
  ) => K1Table label 'False context 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns =
    fromColumns1 (fromK1Columns @"Just" @_ @_ @_ @structure) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns1 (toK1Columns @"Just" @_ @_ @_ @structure)


instance
  ( K1Table label 'True Aggregation (IsStructure structure) structure a
  , Table Aggregation a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ NonEmptyTable a
  ) => K1Table label 'True Aggregation 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( K1Table label 'True DB (IsStructure structure) structure a
  , Table DB a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ NonEmptyTable a
  ) => K1Table label 'True DB 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( K1Table label 'True Insert (IsStructure structure) structure a
  , Table Insert a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ NonEmptyTable a
  ) => K1Table label 'True Insert 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( K1Table label 'True Result (IsStructure structure) structure a
  , HTable (K1Columns label structure)
  , as ~ NonEmpty a
  ) => K1Table label 'True Result 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fmap (fromK1Columns @label @'True @Result @_ @structure) . fromHNonEmptyTable
  toK1Columns = toHNonEmptyTable . fmap (toK1Columns @label @'True @Result @_ @structure)


instance
  ( K1Table label 'False context (IsStructure structure) structure a
  , IsSpecialContext context ~ 'False
  , Table context a
  , K1Columns label structure ~ HLabel label (Columns a)
  , Labelable context
  , as ~ NonEmptyTable a
  ) => K1Table label 'False context 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel unlabeler)
  toK1Columns = hrelabel (hlabel labeler) . toColumns


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" 'True Aggregation (IsStructure structure1) structure1 a
  , K1Table "There" 'True Aggregation (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  ) => K1Table label 'True Aggregation 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Here" @_ @_ @_ @structure1)
      (fromK1Columns @"There" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Here" @_ @_ @_ @structure1)
      (toK1Columns @"There" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" 'True DB (IsStructure structure1) structure1 a
  , K1Table "There" 'True DB (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  ) => K1Table label 'True DB 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Here" @_ @_ @_ @structure1)
      (fromK1Columns @"There" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Here" @_ @_ @_ @structure1)
      (toK1Columns @"There" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" 'True Insert (IsStructure structure1) structure1 a
  , K1Table "There" 'True Insert (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  ) => K1Table label 'True Insert 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Here" @_ @_ @_ @structure1)
      (fromK1Columns @"There" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Here" @_ @_ @_ @structure1)
      (toK1Columns @"There" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" 'True Result (IsStructure structure1) structure1 a
  , K1Table "There" 'True Result (IsStructure structure2) structure2 b
  , e ~ These a b
  ) => K1Table label 'True Result 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns
    = bimap
        (fromK1Columns @"Here" @_ @_ @_ @structure1)
        (fromK1Columns @"There" @_ @_ @_ @structure2)
    . fromHTheseTable
    . hunlabel unlabeler
  toK1Columns
    = hlabel labeler
    . toHTheseTable
    . bimap
        (toK1Columns @"Here" @_ @_ @_ @structure1)
        (toK1Columns @"There" @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" 'False context (IsStructure structure1) structure1 a
  , K1Table "There" 'False context (IsStructure structure2) structure2 b
  , IsSpecialContext context ~ 'False
  , Labelable context
  , Nullifiable context
  , e ~ TheseTable a b
  ) => K1Table label 'False context 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Here" @_ @_ @_ @structure1)
      (fromK1Columns @"There" @_ @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Here" @_ @_ @_ @structure1)
      (toK1Columns @"There" @_ @_ @_ @structure2)



instance
  ( K1Table "fst" isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table "snd" isSpecialContext context (IsStructure structure2) structure2 a2
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , Labelable context
  , a ~ (a1, a2)
  ) => K1Table label isSpecialContext context 'True (structure1, structure2) a
 where
  fromK1Columns (hunlabel unlabeler -> (HPair a b)) =
    ( fromK1Columns @"fst" @_ @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @_ @structure2 b
    )
  toK1Columns (a, b) = hlabel labeler $ HPair
    { hfst = toK1Columns @"fst" @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @_ @structure2 b
    }


instance
  ( K1Table "fst" isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table "snd" isSpecialContext context (IsStructure structure2) structure2 a2
  , K1Table "trd" isSpecialContext context (IsStructure structure3) structure3 a3
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , HTable (K1Columns "trd" structure3)
  , Labelable context
  , a ~ (a1, a2, a3)
  ) => K1Table label isSpecialContext context 'True (structure1, structure2, structure3) a
 where
  fromK1Columns (hunlabel unlabeler -> (HTrio a b c)) =
    ( fromK1Columns @"fst" @_ @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @_ @structure2 b
    , fromK1Columns @"trd" @_ @_ @_ @structure3 c
    )
  toK1Columns (a, b, c) = hlabel labeler $ HTrio
    { hfst = toK1Columns @"fst" @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @_ @structure2 b
    , htrd = toK1Columns @"trd" @_ @_ @_ @structure3 c
    }


instance
  ( K1Table "fst" isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table "snd" isSpecialContext context (IsStructure structure2) structure2 a2
  , K1Table "trd" isSpecialContext context (IsStructure structure3) structure3 a3
  , K1Table "frt" isSpecialContext context (IsStructure structure3) structure4 a4
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , HTable (K1Columns "trd" structure3)
  , HTable (K1Columns "frt" structure4)
  , Labelable context
  , a ~ (a1, a2, a3, a4)
  ) => K1Table label isSpecialContext context 'True (structure1, structure2, structure3, structure4) a
 where
  fromK1Columns (hunlabel unlabeler -> (HQuartet a b c d)) =
    ( fromK1Columns @"fst" @_ @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @_ @structure2 b
    , fromK1Columns @"trd" @_ @_ @_ @structure3 c
    , fromK1Columns @"frt" @_ @_ @_ @structure4 d
    )
  toK1Columns (a, b, c, d) = hlabel labeler $ HQuartet
    { hfst = toK1Columns @"fst" @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @_ @structure2 b
    , htrd = toK1Columns @"trd" @_ @_ @_ @structure3 c
    , hfrt = toK1Columns @"frt" @_ @_ @_ @structure4 d
    }


instance
  ( K1Table "fst" isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table "snd" isSpecialContext context (IsStructure structure2) structure2 a2
  , K1Table "trd" isSpecialContext context (IsStructure structure3) structure3 a3
  , K1Table "frt" isSpecialContext context (IsStructure structure3) structure4 a4
  , K1Table "fft" isSpecialContext context (IsStructure structure3) structure5 a5
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , HTable (K1Columns "trd" structure3)
  , HTable (K1Columns "frt" structure4)
  , HTable (K1Columns "fft" structure5)
  , Labelable context
  , a ~ (a1, a2, a3, a4, a5)
  ) => K1Table label isSpecialContext context 'True (structure1, structure2, structure3, structure4, structure5) a
 where
  fromK1Columns (hunlabel unlabeler -> (HQuintet a b c d e)) =
    ( fromK1Columns @"fst" @_ @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @_ @structure2 b
    , fromK1Columns @"trd" @_ @_ @_ @structure3 c
    , fromK1Columns @"frt" @_ @_ @_ @structure4 d
    , fromK1Columns @"fft" @_ @_ @_ @structure5 e
    )
  toK1Columns (a, b, c, d, e) = hlabel labeler $ HQuintet
    { hfst = toK1Columns @"fst" @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @_ @structure2 b
    , htrd = toK1Columns @"trd" @_ @_ @_ @structure3 c
    , hfrt = toK1Columns @"frt" @_ @_ @_ @structure4 d
    , hfft = toK1Columns @"fft" @_ @_ @_ @structure5 e
    }


instance
  ( IsSpecialContext context ~ isSpecialContext
  , IsStructure structure ~ 'False
  , K1Columns label structure ~ HLabel label (Columns structure)
  , Columns structure ~ Columns a
  , Labelable context
  , Table context a
  ) => K1Table label isSpecialContext context 'False structure a
 where
  fromK1Columns = fromColumns . hunlabel unlabeler
  toK1Columns = hlabel labeler . toColumns
