{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

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
  )
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
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Maybe ( HMaybeTable )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.These ( HTheseTable )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.Recontextualize ( Recontextualize )
import Rel8.Schema.Spec ( Spec( Spec ), KTable )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Schema.Structure
  ( IsStructure, Shape(..), Shape1, Shape2
  , Structure
  )
import Rel8.Schema.Value ( Value( NullableValue, NonNullableValue ) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
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
  Table (t context)
 where
  type Columns (t context) = GRep t
  type Context (t context) = context
  fromColumns = gfromColumns
  toColumns = gtoColumns


instance
  ( Rel8able t
  , TableHelper (IsSpecialContext from) from
  , TableHelper (IsSpecialContext to) to
  ) => Recontextualize from to (t from) (t to)


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


instance (IsSpecialContext context ~ 'False, Nullifiable context) =>
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

  fromUnspecialColumns :: (IsSpecialContext context ~ 'False, Nullifiable context)
    => GRep t (H context) -> t context
  toUnspecialColumns :: (IsSpecialContext context ~ 'False, Nullifiable context)
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
  GColumns (M1 _ _ structure) = GColumns structure
  GColumns (a :*: b) = HPair (GColumns a) (GColumns b)
  GColumns (K1 _ structure) = K1Columns structure


type GRel8able :: Kind.Context -> (Type -> Type) -> (Type -> Type) -> Constraint
class GRel8able context structure rep where
  fromGColumns :: GColumns structure (H context) -> rep blueprint
  toGColumns :: rep blueprint -> GColumns structure (H context)


instance GRel8able context structure rep => GRel8able context (M1 i c structure) (M1 i c rep) where
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
  ( isSpecialContext ~ IsSpecialContext context
  , isStructure ~ IsStructure structure
  , K1Table isSpecialContext context isStructure structure a
  ) => GRel8able context (K1 i structure) (K1 i a)
 where
  fromGColumns = K1 . fromK1Columns @_ @_ @_ @structure
  toGColumns (K1 a) = toK1Columns @_ @_ @_ @structure a


type K1Columns :: Type -> HKTable
type family K1Columns structure where
  K1Columns (Shape1 'Column spec) = HIdentity spec
  K1Columns (Shape2 'Either a b) = HEitherTable (K1Columns a) (K1Columns b)
  K1Columns (Shape1 'List a) = HListTable (K1Columns a)
  K1Columns (Shape1 'Maybe a) = HMaybeTable (K1Columns a)
  K1Columns (Shape1 'NonEmpty a) = HNonEmptyTable (K1Columns a)
  K1Columns (Shape2 'These a b) = HTheseTable (K1Columns a) (K1Columns b)
  K1Columns (a, b) = HPair (K1Columns a) (K1Columns b)
  K1Columns (a, b, c) = HTrio (K1Columns a) (K1Columns b) (K1Columns c)
  K1Columns (a, b, c, d) = HQuartet (K1Columns a) (K1Columns b) (K1Columns c) (K1Columns d)
  K1Columns (a, b, c, d, e) = HQuintet (K1Columns a) (K1Columns b) (K1Columns c) (K1Columns d) (K1Columns e)
  K1Columns a = Columns a


type K1Table :: Bool -> Kind.Context -> Bool -> Type -> Type -> Constraint
class
  ( isSpecialContext ~ IsSpecialContext context
  , isStructure ~ IsStructure structure
  ) => K1Table isSpecialContext context isStructure structure a
 where
  fromK1Columns :: K1Columns structure (H context) -> a
  toK1Columns :: a -> K1Columns structure (H context)


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , a ~ Field Aggregation necessity nullability blueprint
  ) => K1Table 'True Aggregation 'True (Shape1 'Column spec) a
 where
  fromK1Columns (HIdentity (Aggregation a)) = a
  toK1Columns = HIdentity . Aggregation


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , a ~ Field DB necessity nullability blueprint
  ) => K1Table 'True DB 'True (Shape1 'Column spec) a
 where
  fromK1Columns (HIdentity (DB a)) = a
  toK1Columns = HIdentity . DB


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , a ~ Field Insert necessity nullability blueprint
  , KnownNecessity necessity
  ) => K1Table 'True Insert 'True (Shape1 'Column spec) a
 where
  fromK1Columns (HIdentity insert) = case insert of
    RequiredInsert a -> a
    OptionalInsert ma -> ma
  toK1Columns a = HIdentity $ case necessitySing @necessity of
    SRequired -> RequiredInsert a
    SOptional -> OptionalInsert a


instance
  ( spec ~ 'Spec necessity nullability blueprint
  , a ~ Field Result necessity nullability blueprint
  , KnownNullability nullability
  ) => K1Table 'True Result 'True (Shape1 'Column spec) a
 where
  fromK1Columns (HIdentity (Result value)) = case value of
    NullableValue ma -> ma
    NonNullableValue a -> a
  toK1Columns a = HIdentity $ Result $ case nullabilitySing @nullability of
    SNullable -> NullableValue a
    SNonNullable -> NonNullableValue a


instance
  ( IsSpecialContext context ~ 'False
  , a ~ context spec
  ) => K1Table 'False context 'True (Shape1 'Column spec) a
 where
  fromK1Columns = unHIdentity
  toK1Columns = HIdentity


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True Aggregation (IsStructure structure1) structure1 a
  , K1Table 'True Aggregation (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  ) => K1Table 'True Aggregation 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True DB (IsStructure structure1) structure1 a
  , K1Table 'True DB (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  ) => K1Table 'True DB 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True Insert (IsStructure structure1) structure1 a
  , K1Table 'True Insert (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  ) => K1Table 'True Insert 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True Result (IsStructure structure1) structure1 a
  , K1Table 'True Result (IsStructure structure2) structure2 b
  , e ~ Either a b
  ) => K1Table 'True Result 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns
    = bimap
        (fromK1Columns @_ @_ @_ @structure1)
        (fromK1Columns @_ @_ @_ @structure2)
    . fromHEitherTable
  toK1Columns
    = toHEitherTable
    . bimap
        (toK1Columns @_ @_ @_ @structure1)
        (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'False context (IsStructure structure1) structure1 a
  , K1Table 'False context (IsStructure structure2) structure2 b
  , IsSpecialContext context ~ 'False
  , Nullifiable context
  , e ~ EitherTable a b
  ) => K1Table 'False context 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( K1Table 'True Aggregation (IsStructure structure) structure a
  , Table a, Context a ~ Aggregation
  , K1Columns structure ~ Columns a
  , as ~ ListTable a
  ) => K1Table 'True Aggregation 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( K1Table 'True DB (IsStructure structure) structure a
  , Table a, Context a ~ DB
  , K1Columns structure ~ Columns a
  , as ~ ListTable a
  ) => K1Table 'True DB 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( K1Table 'True Insert (IsStructure structure) structure a
  , Table a, Context a ~ Insert
  , K1Columns structure ~ Columns a
  , as ~ ListTable a
  ) => K1Table 'True Insert 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( K1Table 'True Result (IsStructure structure) structure a
  , HTable (K1Columns structure)
  , as ~ [a]
  ) => K1Table 'True Result 'True (Shape1 'List structure) as
 where
  fromK1Columns = fmap (fromK1Columns @'True @Result @_ @structure) . fromHListTable
  toK1Columns = toHListTable . fmap (toK1Columns @'True @Result @_ @structure)


instance
  ( K1Table 'False context (IsStructure structure) structure a
  , IsSpecialContext context ~ 'False
  , Table a, Context a ~ context
  , K1Columns structure ~ Columns a
  , as ~ ListTable a
  ) => K1Table 'False context 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( HTable (K1Columns structure)
  , K1Table 'True Aggregation (IsStructure structure) structure a
  , ma ~ MaybeTable a
  ) => K1Table 'True Aggregation 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns = fromColumns1 (fromK1Columns @_ @_ @_ @structure)
  toK1Columns = toColumns1 (toK1Columns @_ @_ @_ @structure)


instance
  ( HTable (K1Columns structure)
  , K1Table 'True DB (IsStructure structure) structure a
  , ma ~ MaybeTable a
  ) => K1Table 'True DB 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns = fromColumns1 (fromK1Columns @_ @_ @_ @structure)
  toK1Columns = toColumns1 (toK1Columns @_ @_ @_ @structure)


instance
  ( HTable (K1Columns structure)
  , K1Table 'True Insert (IsStructure structure) structure a
  , ma ~ MaybeTable a
  ) => K1Table 'True Insert 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns = fromColumns1 (fromK1Columns @_ @_ @_ @structure)
  toK1Columns = toColumns1 (toK1Columns @_ @_ @_ @structure)


instance
  ( K1Table 'True Result (IsStructure structure) structure a
  , HTable (K1Columns structure)
  , ma ~ Maybe a
  ) => K1Table 'True Result 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns = fmap (fromK1Columns @'True @Result @_ @structure) . fromHMaybeTable
  toK1Columns = toHMaybeTable . fmap (toK1Columns @'True @Result @_ @structure)


instance
  ( HTable (K1Columns structure)
  , K1Table 'False context (IsStructure structure) structure a
  , IsSpecialContext context ~ 'False
  , Nullifiable context
  , ma ~ MaybeTable a
  ) => K1Table 'False context 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns = fromColumns1 (fromK1Columns @_ @_ @_ @structure)
  toK1Columns = toColumns1 (toK1Columns @_ @_ @_ @structure)


instance
  ( K1Table 'True Aggregation (IsStructure structure) structure a
  , Table a, Context a ~ Aggregation
  , K1Columns structure ~ Columns a
  , as ~ NonEmptyTable a
  ) => K1Table 'True Aggregation 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( K1Table 'True DB (IsStructure structure) structure a
  , Table a, Context a ~ DB
  , K1Columns structure ~ Columns a
  , as ~ NonEmptyTable a
  ) => K1Table 'True DB 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( K1Table 'True Insert (IsStructure structure) structure a
  , Table a, Context a ~ Insert
  , K1Columns structure ~ Columns a
  , as ~ NonEmptyTable a
  ) => K1Table 'True Insert 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( K1Table 'True Result (IsStructure structure) structure a
  , HTable (K1Columns structure)
  , as ~ NonEmpty a
  ) => K1Table 'True Result 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fmap (fromK1Columns @'True @Result @_ @structure) . fromHNonEmptyTable
  toK1Columns = toHNonEmptyTable . fmap (toK1Columns @'True @Result @_ @structure)


instance
  ( K1Table 'False context (IsStructure structure) structure a
  , IsSpecialContext context ~ 'False
  , Table a, Context a ~ context
  , K1Columns structure ~ Columns a
  , as ~ NonEmptyTable a
  ) => K1Table 'False context 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True Aggregation (IsStructure structure1) structure1 a
  , K1Table 'True Aggregation (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  ) => K1Table 'True Aggregation 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True DB (IsStructure structure1) structure1 a
  , K1Table 'True DB (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  ) => K1Table 'True DB 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True Insert (IsStructure structure1) structure1 a
  , K1Table 'True Insert (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  ) => K1Table 'True Insert 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'True Result (IsStructure structure1) structure1 a
  , K1Table 'True Result (IsStructure structure2) structure2 b
  , e ~ These a b
  ) => K1Table 'True Result 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns
    = bimap
        (fromK1Columns @_ @_ @_ @structure1)
        (fromK1Columns @_ @_ @_ @structure2)
    . fromHTheseTable
  toK1Columns
    = toHTheseTable
    . bimap
        (toK1Columns @_ @_ @_ @structure1)
        (toK1Columns @_ @_ @_ @structure2)


instance
  ( HTable (K1Columns structure1)
  , HTable (K1Columns structure2)
  , K1Table 'False context (IsStructure structure1) structure1 a
  , K1Table 'False context (IsStructure structure2) structure2 b
  , IsSpecialContext context ~ 'False
  , Nullifiable context
  , e ~ TheseTable a b
  ) => K1Table 'False context 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @_ @_ @_ @structure1)
      (fromK1Columns @_ @_ @_ @structure2)
  toK1Columns =
    toColumns2
      (toK1Columns @_ @_ @_ @structure1)
      (toK1Columns @_ @_ @_ @structure2)


instance
  ( K1Table isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table isSpecialContext context (IsStructure structure2) structure2 a2
  , a ~ (a1, a2)
  ) => K1Table isSpecialContext context 'True (structure1, structure2) a
 where
  fromK1Columns (HPair a b) =
    ( fromK1Columns @_ @_ @_ @structure1 a
    , fromK1Columns @_ @_ @_ @structure2 b
    )
  toK1Columns (a, b) = HPair
    { hfst = toK1Columns @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @_ @_ @_ @structure2 b
    }


instance
  ( K1Table isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table isSpecialContext context (IsStructure structure2) structure2 a2
  , K1Table isSpecialContext context (IsStructure structure3) structure3 a3
  , a ~ (a1, a2, a3)
  ) => K1Table isSpecialContext context 'True (structure1, structure2, structure3) a
 where
  fromK1Columns (HTrio a b c) =
    ( fromK1Columns @_ @_ @_ @structure1 a
    , fromK1Columns @_ @_ @_ @structure2 b
    , fromK1Columns @_ @_ @_ @structure3 c
    )
  toK1Columns (a, b, c) = HTrio
    { hfst = toK1Columns @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @_ @_ @_ @structure2 b
    , htrd = toK1Columns @_ @_ @_ @structure3 c
    }


instance
  ( K1Table isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table isSpecialContext context (IsStructure structure2) structure2 a2
  , K1Table isSpecialContext context (IsStructure structure3) structure3 a3
  , K1Table isSpecialContext context (IsStructure structure3) structure4 a4
  , a ~ (a1, a2, a3, a4)
  ) => K1Table isSpecialContext context 'True (structure1, structure2, structure3, structure4) a
 where
  fromK1Columns (HQuartet a b c d) =
    ( fromK1Columns @_ @_ @_ @structure1 a
    , fromK1Columns @_ @_ @_ @structure2 b
    , fromK1Columns @_ @_ @_ @structure3 c
    , fromK1Columns @_ @_ @_ @structure4 d
    )
  toK1Columns (a, b, c, d) = HQuartet
    { hfst = toK1Columns @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @_ @_ @_ @structure2 b
    , htrd = toK1Columns @_ @_ @_ @structure3 c
    , hfrt = toK1Columns @_ @_ @_ @structure4 d
    }


instance
  ( K1Table isSpecialContext context (IsStructure structure1) structure1 a1
  , K1Table isSpecialContext context (IsStructure structure2) structure2 a2
  , K1Table isSpecialContext context (IsStructure structure3) structure3 a3
  , K1Table isSpecialContext context (IsStructure structure3) structure4 a4
  , K1Table isSpecialContext context (IsStructure structure3) structure5 a5
  , a ~ (a1, a2, a3, a4, a5)
  ) => K1Table isSpecialContext context 'True (structure1, structure2, structure3, structure4, structure5) a
 where
  fromK1Columns (HQuintet a b c d e) =
    ( fromK1Columns @_ @_ @_ @structure1 a
    , fromK1Columns @_ @_ @_ @structure2 b
    , fromK1Columns @_ @_ @_ @structure3 c
    , fromK1Columns @_ @_ @_ @structure4 d
    , fromK1Columns @_ @_ @_ @structure5 e
    )
  toK1Columns (a, b, c, d, e) = HQuintet
    { hfst = toK1Columns @_ @_ @_ @structure1 a
    , hsnd = toK1Columns @_ @_ @_ @structure2 b
    , htrd = toK1Columns @_ @_ @_ @structure3 c
    , hfrt = toK1Columns @_ @_ @_ @structure4 d
    , hfft = toK1Columns @_ @_ @_ @structure5 e
    }


instance
  ( IsSpecialContext context ~ isSpecialContext
  , IsStructure structure ~ 'False
  , K1Columns structure ~ Columns structure
  , Columns structure ~ Columns a
  , Table a, Context a ~ context
  ) => K1Table isSpecialContext context 'False structure a
 where
  fromK1Columns = fromColumns
  toK1Columns = toColumns
