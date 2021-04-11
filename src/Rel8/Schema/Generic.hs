{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
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
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics
  ( Generic, Rep, from, to
  , (:*:)( (:*:) ), K1( K1 ), M1( M1 )
  , D, C, S
  , Meta( MetaSel )
  )
import GHC.TypeLits ( Symbol, KnownSymbol )
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Kind.Necessity
  ( SNecessity( SRequired, SOptional )
  , KnownNecessity, necessitySing
  )
import Rel8.Schema.Context ( Col(..) )
import Rel8.Schema.Context.Identity
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
import Rel8.Schema.Context.Label
  ( Labelable, labeler, unlabeler
  , hlabeler, hunlabeler
  )
import Rel8.Schema.Field ( Field )
import Rel8.Schema.HTable ( HTable )
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
import Rel8.Schema.Insert ( Insert, Col(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name( Name ), Col(..) )
import Rel8.Schema.Spec ( Spec( Spec ), KTable )
import Rel8.Schema.Structure
  ( IsStructure, Shape(..), Shape1, Shape2
  , Structure
  )
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


instance (Rel8able t, TableHelper context) => Table context (t context) where
  type Columns (t context) = GRep t
  type Context (t context) = context
  fromColumns = gfromColumns
  toColumns = gtoColumns


type TableHelper :: K.Context -> Constraint
class TableHelper context where
   gfromColumns :: Rel8able t => GRep t (Col context) -> t context
   gtoColumns :: Rel8able t => t context -> GRep t (Col context)


instance TableHelper Expr where
  gfromColumns = fromExprColumns
  gtoColumns = toExprColumns


instance TableHelper Insert where
  gfromColumns = fromInsertColumns
  gtoColumns = toInsertColumns


instance TableHelper Identity where
  gfromColumns = fromIdentityColumns
  gtoColumns = toIdentityColumns


instance TableHelper Name where
  gfromColumns = fromNameColumns
  gtoColumns = toNameColumns


-- | This type class allows you to define custom 'Table's using higher-kinded
-- data types. Higher-kinded data types are data types of the pattern:
--
-- @
-- data MyType f =
--   MyType { field1 :: Column f T1 OR HK1 f
--          , field2 :: Column f T2 OR HK2 f
--          , ...
--          , fieldN :: Column f Tn OR HKn f
--          }
-- @
-- 
-- where @Tn@ is any Haskell type, and @HKn@ is any higher-kinded type.
-- 
-- That is, higher-kinded data are records where all fields in the record are
-- all either of the type @Column f T@ (for any @T@), or are themselves
-- higher-kinded data:
-- 
-- [Nested]
-- 
-- @
-- data Nested f =
--   Nested { nested1 :: MyType f
--          , nested2 :: MyType f
--          }
-- @
-- 
-- The @Rel8able@ type class is used to give us a special mapping operation
-- that lets us change the type parameter @f@.
-- 
-- [Supplying @Rel8able@ instances]
-- 
-- This type class should be derived generically for all table types in your
-- project. To do this, enable the @DeriveAnyType@ and @DeriveGeneric@ language
-- extensions:
-- 
-- @
-- \{\-\# LANGUAGE DeriveAnyClass, DeriveGeneric #-\}
-- 
-- data MyType f = MyType { fieldA :: Column f T }
--   deriving ( GHC.Generics.Generic, Rel8able )
-- @
type Rel8able :: KTable -> Constraint
class HTable (GRep t) => Rel8able t where
  type GRep t :: K.HTable

  fromExprColumns :: GRep t (Col Expr) -> t Expr
  toExprColumns :: t Expr -> GRep t (Col Expr)

  fromInsertColumns :: GRep t (Col Insert) -> t Insert
  toInsertColumns :: t Insert -> GRep t (Col Insert)

  fromIdentityColumns :: GRep t (Col Identity) -> t Identity
  toIdentityColumns :: t Identity -> GRep t (Col Identity)

  fromNameColumns :: GRep t (Col Name) -> t Name
  toNameColumns :: t Name -> GRep t (Col Name)

  type GRep t = GColumns (Rep (t Structure))

  default fromExprColumns ::
    ( Generic (t Expr)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Expr (Rep (t Structure)) (Rep (t Expr))
    ) => GRep t (Col Expr) -> t Expr
  fromExprColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toExprColumns ::
    ( Generic (t Expr)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Expr (Rep (t Structure)) (Rep (t Expr))
    ) => t Expr -> GRep t (Col Expr)
  toExprColumns = toGColumns @_ @(Rep (t Structure)) . from

  default fromInsertColumns ::
    ( Generic (t Insert)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Insert (Rep (t Structure)) (Rep (t Insert))
    ) => GRep t (Col Insert) -> t Insert
  fromInsertColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toInsertColumns ::
    ( Generic (t Insert)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Insert (Rep (t Structure)) (Rep (t Insert))
    ) => t Insert -> GRep t (Col Insert)
  toInsertColumns = toGColumns @_ @(Rep (t Structure)) . from

  default fromIdentityColumns ::
    ( Generic (t Identity)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Identity (Rep (t Structure)) (Rep (t Identity))
    ) => GRep t (Col Identity) -> t Identity
  fromIdentityColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toIdentityColumns ::
    ( Generic (t Identity)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Identity (Rep (t Structure)) (Rep (t Identity))
    ) => t Identity -> GRep t (Col Identity)
  toIdentityColumns = toGColumns @_ @(Rep (t Structure)) . from

  default fromNameColumns ::
    ( Generic (t Name)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Name (Rep (t Structure)) (Rep (t Name))
    ) => GRep t (Col Name) -> t Name
  fromNameColumns = to . fromGColumns @_ @(Rep (t Structure))

  default toNameColumns ::
    ( Generic (t Name)
    , GColumns (Rep (t Structure)) ~ GRep t
    , GRel8able Name (Rep (t Structure)) (Rep (t Name))
    ) => t Name -> GRep t (Col Name)
  toNameColumns = toGColumns @_ @(Rep (t Structure)) . from


type GColumns :: (Type -> Type) -> K.HTable
type family GColumns structure where
  GColumns (M1 D _ structure) = GColumns structure
  GColumns (M1 C _ structure) = GColumns structure
  GColumns (a :*: b) = HPair (GColumns a) (GColumns b)
  GColumns (M1 S ('MetaSel ('Just label) _ _ _) (K1 _ structure)) =
    K1Columns label structure


type GRel8able :: K.Context -> (Type -> Type) -> (Type -> Type) -> Constraint
class GRel8able context structure rep where
  fromGColumns :: GColumns structure (Col context) -> rep x
  toGColumns :: rep x -> GColumns structure (Col context)


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
  ( K1Table label context isStructure structure a
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , structureK1 ~ K1 i structure
  , k1 ~ K1 i a
  ) => GRel8able context (M1 S meta structureK1) (M1 S meta k1)
 where
  fromGColumns = M1 . K1 . fromK1Columns @label @_ @_ @structure
  toGColumns (M1 (K1 a)) = toK1Columns @label @_ @_ @structure a


type K1Columns :: Symbol -> Type -> K.HTable
type family K1Columns label structure where
  K1Columns label (Shape1 'Column ('Spec '[] necessity a)) =
    HIdentity ('Spec '[label] necessity a)
  K1Columns _label (Shape1 'Column ('Spec (label ': labels) necessity a)) =
    HIdentity ('Spec (label ': labels) necessity a)
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


type K1Table :: Symbol -> K.Context -> Bool -> Type -> Type -> Constraint
class isStructure ~ IsStructure structure =>
  K1Table label context isStructure structure a
 where
  fromK1Columns :: K1Columns label structure (Col context) -> a
  toK1Columns :: a -> K1Columns label structure (Col context)


instance
  ( x ~ Field Expr '[] necessity a
  ) => K1Table label Expr 'True (Shape1 'Column ('Spec '[] necessity a)) x
 where
  fromK1Columns (HIdentity (DB a)) = a
  toK1Columns = HIdentity . DB
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( x ~ Field Expr (label ': labels) necessity a
  ) => K1Table _label Expr 'True (Shape1 'Column ('Spec (label ': labels) necessity a)) x
 where
  fromK1Columns (HIdentity (DB a)) = a
  toK1Columns = HIdentity . DB
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( x ~ Field Insert '[] necessity a
  , KnownNecessity necessity
  ) => K1Table label Insert 'True (Shape1 'Column ('Spec '[] necessity a)) x
 where
  fromK1Columns (HIdentity insert) = case insert of
    RequiredInsert a -> a
    OptionalInsert ma -> ma
  toK1Columns a = HIdentity $ case necessitySing @necessity of
    SRequired -> RequiredInsert a
    SOptional -> OptionalInsert a
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( x ~ Field Insert (label ': labels) necessity a
  , KnownNecessity necessity
  ) => K1Table _label Insert 'True (Shape1 'Column ('Spec (label ': labels) necessity a)) x
 where
  fromK1Columns (HIdentity insert) = case insert of
    RequiredInsert a -> a
    OptionalInsert ma -> ma
  toK1Columns a = HIdentity $ case necessitySing @necessity of
    SRequired -> RequiredInsert a
    SOptional -> OptionalInsert a
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( x ~ Field Identity '[] necessity a
  ) => K1Table label Identity 'True (Shape1 'Column ('Spec '[] necessity a)) x
 where
  fromK1Columns (HIdentity (Result a)) = a
  toK1Columns = HIdentity . Result
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( x ~ Field Identity (label ': labels) necessity a
  ) => K1Table _label Identity 'True (Shape1 'Column ('Spec (label ': labels) necessity a)) x
 where
  fromK1Columns (HIdentity (Result a)) = a
  toK1Columns = HIdentity . Result
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( x ~ Field Name '[] necessity a
  ) => K1Table _label Name 'True (Shape1 'Column ('Spec (label ': labels) necessity a)) x
 where
  fromK1Columns (HIdentity (NameCol a)) = Name a
  toK1Columns (Name a) = HIdentity (NameCol a)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( x ~ Field Name (label ': labels) necessity a
  ) => K1Table label Name 'True (Shape1 'Column ('Spec '[] necessity a)) x
 where
  fromK1Columns = Name . (\(NameCol a) -> a) . unlabeler . unHIdentity
  toK1Columns = HIdentity . labeler . NameCol . (\(Name a) -> a)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" Expr (IsStructure structure1) structure1 a
  , K1Table "Right" Expr (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  , KnownSymbol label
  ) => K1Table label Expr 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Left" @_ @_ @structure1)
      (fromK1Columns @"Right" @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Left" @_ @_ @structure1)
      (toK1Columns @"Right" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" Insert (IsStructure structure1) structure1 a
  , K1Table "Right" Insert (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  , KnownSymbol label
  ) => K1Table label Insert 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Left" @_ @_ @structure1)
      (fromK1Columns @"Right" @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Left" @_ @_ @structure1)
      (toK1Columns @"Right" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" Identity (IsStructure structure1) structure1 a
  , K1Table "Right" Identity (IsStructure structure2) structure2 b
  , e ~ Either a b
  , KnownSymbol label
  ) => K1Table label Identity 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns
    = bimap
        (fromK1Columns @"Left" @_ @_ @structure1)
        (fromK1Columns @"Right" @_ @_ @structure2)
    . fromHEitherTable
    . hunlabel unlabeler
  toK1Columns
    = hlabel labeler
    . toHEitherTable
    . bimap
        (toK1Columns @"Left" @_ @_ @structure1)
        (toK1Columns @"Right" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Left" structure1)
  , HTable (K1Columns "Right" structure2)
  , K1Table "Left" Name (IsStructure structure1) structure1 a
  , K1Table "Right" Name (IsStructure structure2) structure2 b
  , e ~ EitherTable a b
  , KnownSymbol label
  ) => K1Table label Name 'True (Shape2 'Either structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Left" @_ @_ @structure1)
      (fromK1Columns @"Right" @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Left" @_ @_ @structure1)
      (toK1Columns @"Right" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( Table Expr a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ ListTable a
  , KnownSymbol label
  ) => K1Table label Expr 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel hunlabeler)
  toK1Columns = hrelabel (hlabel hlabeler) . toColumns
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( Table Insert a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ ListTable a
  , KnownSymbol label
  ) => K1Table label Insert 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel hunlabeler)
  toK1Columns = hrelabel (hlabel hlabeler) . toColumns
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( K1Table label Identity (IsStructure structure) structure a
  , HTable (K1Columns label structure)
  , as ~ [a]
  ) => K1Table label Identity 'True (Shape1 'List structure) as
 where
  fromK1Columns = fmap (fromK1Columns @label @_ @_ @structure) . fromHListTable
  toK1Columns = toHListTable . fmap (toK1Columns @label @_ @_ @structure)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( Table Name a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ ListTable a
  , KnownSymbol label
  ) => K1Table label Name 'True (Shape1 'List structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel hunlabeler)
  toK1Columns = hrelabel (hlabel hlabeler) . toColumns
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" Expr (IsStructure structure) structure a
  , ma ~ MaybeTable a
  , KnownSymbol label
  ) => K1Table label Expr 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns =
    fromColumns1 (fromK1Columns @"Just" @_ @_ @structure) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns1 (toK1Columns @"Just" @_ @_ @structure)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" Insert (IsStructure structure) structure a
  , ma ~ MaybeTable a
  , KnownSymbol label
  ) => K1Table label Insert 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns =
    fromColumns1 (fromK1Columns @"Just" @_ @_ @structure) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns1 (toK1Columns @"Just" @_ @_ @structure)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" Identity (IsStructure structure) structure a
  , ma ~ Maybe a
  , KnownSymbol label
  ) => K1Table label Identity 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns
    = fmap (fromK1Columns @"Just" @_ @_ @structure)
    . fromHMaybeTable
    . hunlabel unlabeler
  toK1Columns
    = hlabel labeler
    . toHMaybeTable
    . fmap (toK1Columns @"Just" @_ @_ @structure)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Just" structure)
  , K1Table "Just" Name (IsStructure structure) structure a
  , ma ~ MaybeTable a
  , KnownSymbol label
  ) => K1Table label Name 'True (Shape1 'Maybe structure) ma
 where
  fromK1Columns =
    fromColumns1 (fromK1Columns @"Just" @_ @_ @structure) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns1 (toK1Columns @"Just" @_ @_ @structure)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( Table Expr a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ NonEmptyTable a
  , KnownSymbol label
  ) => K1Table label Expr 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel hunlabeler)
  toK1Columns = hrelabel (hlabel hlabeler) . toColumns
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( Table Insert a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ NonEmptyTable a
  , KnownSymbol label
  ) => K1Table label Insert 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel hunlabeler)
  toK1Columns = hrelabel (hlabel hlabeler) . toColumns
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( K1Table label Identity (IsStructure structure) structure a
  , HTable (K1Columns label structure)
  , as ~ NonEmpty a
  ) => K1Table label Identity 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fmap (fromK1Columns @label @_ @_ @structure) . fromHNonEmptyTable
  toK1Columns = toHNonEmptyTable . fmap (toK1Columns @label @_ @_ @structure)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( Table Name a
  , K1Columns label structure ~ HLabel label (Columns a)
  , as ~ NonEmptyTable a
  , KnownSymbol label
  ) => K1Table label Name 'True (Shape1 'NonEmpty structure) as
 where
  fromK1Columns = fromColumns . hrelabel (hunlabel hunlabeler)
  toK1Columns = hrelabel (hlabel hlabeler) . toColumns
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" Expr (IsStructure structure1) structure1 a
  , K1Table "There" Expr (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  , KnownSymbol label
  ) => K1Table label Expr 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Here" @_ @_ @structure1)
      (fromK1Columns @"There" @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Here" @_ @_ @structure1)
      (toK1Columns @"There" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" Insert (IsStructure structure1) structure1 a
  , K1Table "There" Insert (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  , KnownSymbol label
  ) => K1Table label Insert 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Here" @_ @_ @structure1)
      (fromK1Columns @"There" @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Here" @_ @_ @structure1)
      (toK1Columns @"There" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" Identity (IsStructure structure1) structure1 a
  , K1Table "There" Identity (IsStructure structure2) structure2 b
  , e ~ These a b
  , KnownSymbol label
  ) => K1Table label Identity 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns
    = bimap
        (fromK1Columns @"Here" @_ @_ @structure1)
        (fromK1Columns @"There" @_ @_ @structure2)
    . fromHTheseTable
    . hunlabel unlabeler
  toK1Columns
    = hlabel labeler
    . toHTheseTable
    . bimap
        (toK1Columns @"Here" @_ @_ @structure1)
        (toK1Columns @"There" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( HTable (K1Columns "Here" structure1)
  , HTable (K1Columns "There" structure2)
  , K1Table "Here" Name (IsStructure structure1) structure1 a
  , K1Table "There" Name (IsStructure structure2) structure2 b
  , e ~ TheseTable a b
  , KnownSymbol label
  ) => K1Table label Name 'True (Shape2 'These structure1 structure2) e
 where
  fromK1Columns =
    fromColumns2
      (fromK1Columns @"Here" @_ @_ @structure1)
      (fromK1Columns @"There" @_ @_ @structure2) .
    hunlabel unlabeler
  toK1Columns =
    hlabel labeler .
    toColumns2
      (toK1Columns @"Here" @_ @_ @structure1)
      (toK1Columns @"There" @_ @_ @structure2)
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( K1Table "fst" context (IsStructure structure1) structure1 a1
  , K1Table "snd" context (IsStructure structure2) structure2 a2
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , Labelable context
  , a ~ (a1, a2)
  , KnownSymbol label
  ) => K1Table label context 'True (structure1, structure2) a
 where
  fromK1Columns (hunlabel unlabeler -> (HPair a b)) =
    ( fromK1Columns @"fst" @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @structure2 b
    )
  toK1Columns (a, b) = hlabel labeler $ HPair
    { hfst = toK1Columns @"fst" @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @structure2 b
    }
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( K1Table "fst" context (IsStructure structure1) structure1 a1
  , K1Table "snd" context (IsStructure structure2) structure2 a2
  , K1Table "trd" context (IsStructure structure3) structure3 a3
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , HTable (K1Columns "trd" structure3)
  , Labelable context
  , a ~ (a1, a2, a3)
  , KnownSymbol label
  ) => K1Table label context 'True (structure1, structure2, structure3) a
 where
  fromK1Columns (hunlabel unlabeler -> (HTrio a b c)) =
    ( fromK1Columns @"fst" @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @structure2 b
    , fromK1Columns @"trd" @_ @_ @structure3 c
    )
  toK1Columns (a, b, c) = hlabel labeler $ HTrio
    { hfst = toK1Columns @"fst" @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @structure2 b
    , htrd = toK1Columns @"trd" @_ @_ @structure3 c
    }
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( K1Table "fst" context (IsStructure structure1) structure1 a1
  , K1Table "snd" context (IsStructure structure2) structure2 a2
  , K1Table "trd" context (IsStructure structure3) structure3 a3
  , K1Table "frt" context (IsStructure structure3) structure4 a4
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , HTable (K1Columns "trd" structure3)
  , HTable (K1Columns "frt" structure4)
  , Labelable context
  , a ~ (a1, a2, a3, a4)
  , KnownSymbol label
  ) => K1Table label context 'True (structure1, structure2, structure3, structure4) a
 where
  fromK1Columns (hunlabel unlabeler -> (HQuartet a b c d)) =
    ( fromK1Columns @"fst" @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @structure2 b
    , fromK1Columns @"trd" @_ @_ @structure3 c
    , fromK1Columns @"frt" @_ @_ @structure4 d
    )
  toK1Columns (a, b, c, d) = hlabel labeler $ HQuartet
    { hfst = toK1Columns @"fst" @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @structure2 b
    , htrd = toK1Columns @"trd" @_ @_ @structure3 c
    , hfrt = toK1Columns @"frt" @_ @_ @structure4 d
    }
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( K1Table "fst" context (IsStructure structure1) structure1 a1
  , K1Table "snd" context (IsStructure structure2) structure2 a2
  , K1Table "trd" context (IsStructure structure3) structure3 a3
  , K1Table "frt" context (IsStructure structure3) structure4 a4
  , K1Table "fft" context (IsStructure structure3) structure5 a5
  , HTable (K1Columns "fst" structure1)
  , HTable (K1Columns "snd" structure2)
  , HTable (K1Columns "trd" structure3)
  , HTable (K1Columns "frt" structure4)
  , HTable (K1Columns "fft" structure5)
  , Labelable context
  , a ~ (a1, a2, a3, a4, a5)
  , KnownSymbol label
  ) => K1Table label context 'True (structure1, structure2, structure3, structure4, structure5) a
 where
  fromK1Columns (hunlabel unlabeler -> (HQuintet a b c d e)) =
    ( fromK1Columns @"fst" @_ @_ @structure1 a
    , fromK1Columns @"snd" @_ @_ @structure2 b
    , fromK1Columns @"trd" @_ @_ @structure3 c
    , fromK1Columns @"frt" @_ @_ @structure4 d
    , fromK1Columns @"fft" @_ @_ @structure5 e
    )
  toK1Columns (a, b, c, d, e) = hlabel labeler $ HQuintet
    { hfst = toK1Columns @"fst" @_ @_ @structure1 a
    , hsnd = toK1Columns @"snd" @_ @_ @structure2 b
    , htrd = toK1Columns @"trd" @_ @_ @structure3 c
    , hfrt = toK1Columns @"frt" @_ @_ @structure4 d
    , hfft = toK1Columns @"fft" @_ @_ @structure5 e
    }
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}


instance
  ( IsStructure structure ~ 'False
  , K1Columns label structure ~ HLabel label (Columns structure)
  , Columns structure ~ Columns a
  , Labelable context
  , Table context a
  , KnownSymbol label
  ) => K1Table label context 'False structure a
 where
  fromK1Columns = fromColumns . hunlabel unlabeler
  toK1Columns = hlabel labeler . toColumns
  {-# INLINABLE fromK1Columns #-}
  {-# INLINABLE toK1Columns #-}
