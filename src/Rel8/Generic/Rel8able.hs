{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Rel8able
  ( KRel8able, Rel8able
  , Algebra
  , GRep
  , GColumns, gfromColumns, gtoColumns
  , GFromExprs, gfromResult, gtoResult
  , Lower
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Generic.Map ( GMap )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Generic.Table ( GAlgebra )
import qualified Rel8.Generic.Table.Record as G
import qualified Rel8.Kind.Algebra as K ( Algebra(..) )
import Rel8.Schema.Context.Virtual ( Abstract(..) )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( fromColumns, toColumns, fromResult, toResult
  , TTable, TColumns, TFromExprs
  )


-- | The kind of 'Rel8able' types
type KRel8able :: Type
type KRel8able = K.Rel8able


type Lower :: K.HContext -> K.Context
type family Lower f = g | g -> f where
  Lower Aggregate = Aggregate
  Lower Expr = Expr
  Lower Name = Name


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
type Rel8able :: K.Rel8able -> Constraint
class HTable (GColumns t) => Rel8able t where
  type GColumns t :: K.HTable
  type GFromExprs t :: Type

  gfromColumns :: Abstract context -> GColumns t context -> t (Lower context)
  gtoColumns :: Abstract context -> t (Lower context) -> GColumns t context

  gfromResult :: GColumns t Result -> GFromExprs t
  gtoResult :: GFromExprs t -> GColumns t Result

  type GColumns t = G.GColumns TColumns (GRep t Expr)
  type GFromExprs t = t Result

  default gfromColumns :: forall context.
    ( VRel8able t Aggregate
    , VRel8able t Expr
    , VRel8able t Name
    )
    => Abstract context -> GColumns t context -> t (Lower context)
  gfromColumns = \case
    VAggregate -> vfromColumns
    VExpr -> vfromColumns
    VName -> vfromColumns

  default gtoColumns :: forall context.
    ( VRel8able t Aggregate
    , VRel8able t Expr
    , VRel8able t Name
    )
    => Abstract context -> t (Lower context) -> GColumns t context
  gtoColumns = \case
    VAggregate -> vtoColumns
    VExpr -> vtoColumns
    VName -> vtoColumns

  default gfromResult ::
    ( Generic (Record (t Result))
    , G.GTable (TTable Expr) TColumns TFromExprs (GRep t Expr)
    , G.GColumns TColumns (GRep t Expr) ~ GColumns t
    , Rep (Record (t Result)) ~ GMap TFromExprs (GRep t Expr)
    , GFromExprs t ~ t Result
    )
    => GColumns t Result -> GFromExprs t
  gfromResult =
    unrecord .
    to .
    G.gfromResult
      @(TTable Expr)
      @TColumns
      @TFromExprs
      @(GRep t Expr)
      (\(_ :: proxy x) -> fromResult @Expr @x)

  default gtoResult ::
    ( Generic (Record (t Result))
    , G.GTable (TTable Expr) TColumns TFromExprs (GRep t Expr)
    , G.GColumns TColumns (GRep t Expr) ~ GColumns t
    , Rep (Record (t Result)) ~ GMap TFromExprs (GRep t Expr)
    , GFromExprs t ~ t Result
    )
    => GFromExprs t -> GColumns t Result
  gtoResult =
    G.gtoResult
      @(TTable Expr)
      @TColumns
      @TFromExprs
      @(GRep t Expr)
      (\(_ :: proxy x) -> toResult @Expr @x) .
    from .
    Record


type Algebra :: K.Rel8able -> K.Algebra
type Algebra t = GAlgebra (GRep t Expr)


type GRep :: K.Rel8able -> K.Context -> Type -> Type
type GRep t context = Rep (Record (t context))


type VRel8able :: K.Rel8able -> K.HContext -> Constraint
type VRel8able t context =
  ( Generic (Record (t (Lower context)))
  , G.GTable (TTable context) TColumns TFromExprs (GRep t (Lower context))
  , G.GColumns TColumns (GRep t (Lower context)) ~ GColumns t
  )


vfromColumns :: forall t context. VRel8able t context
  => GColumns t context -> t (Lower context)
vfromColumns =
  unrecord .
  to .
  G.gfromColumns @(TTable context) @TColumns @TFromExprs fromColumns


vtoColumns :: forall t context. VRel8able t context
  => t (Lower context) -> GColumns t context
vtoColumns =
  G.gtoColumns @(TTable context) @TColumns @TFromExprs toColumns .
  from .
  Record
