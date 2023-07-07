{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rel8.Table.ADT (
  ADT (ADT),
  ADTable,
  BuildableADT,
  BuildADT,
  buildADT,
  ConstructableADT,
  ConstructADT,
  constructADT,
  DeconstructADT,
  deconstructADT,
  deconstructAADT,
  NameADT,
  nameADT,
  ADTRep,
)
where

-- base
import Data.Kind (Constraint, Type)
import GHC.Generics (Generic, from, to)
import GHC.TypeLits (Symbol)
import Prelude

-- rel8
import Rel8.Expr (Expr)
import Rel8.FCF (Eval, Exp)
import Rel8.Generic.Construction (
  GGBuild,
  GGBuildable,
  GGConstruct,
  GGConstructable,
  GGDeconstruct,
  GGName,
  ggbuild,
  ggconstruct,
  ggdeconstruct,
  ggdeconstructA,
  ggname,
 )
import Rel8.Generic.Record (Record (Record), unrecord)
import Rel8.Generic.Rel8able (
  GColumns,
  GFromExprs,
  GRep,
  Rel8able,
  TSerialize,
  deserialize,
  gfromColumns,
  gfromResult,
  gtoColumns,
  gtoResult,
  serialize,
 )
import qualified Rel8.Generic.Table.ADT as G
import qualified Rel8.Kind.Algebra as K
import Rel8.Schema.HTable (HTable)
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name (Name)
import Rel8.Schema.Result (Result)
import Rel8.Table (TColumns, Table)

-- semigroupoids
import Data.Functor.Apply (Apply)


type ADT :: K.Rel8able -> K.Rel8able
newtype ADT t context = ADT (GColumnsADT t context)


instance ADTable t => Rel8able (ADT t) where
  type GColumns (ADT t) = GColumnsADT t
  type GFromExprs (ADT t) = t Result


  gfromColumns _ = ADT
  gtoColumns _ (ADT a) = a


  gfromResult =
    unrecord
      . to
      . G.gfromResultADT
        @TSerialize
        @TColumns
        @(Eval (ADTRep t Expr))
        @(Eval (ADTRep t Result))
        (\(_ :: proxy x) -> deserialize @_ @x)


  gtoResult =
    G.gtoResultADT
      @TSerialize
      @TColumns
      @(Eval (ADTRep t Expr))
      @(Eval (ADTRep t Result))
      (\(_ :: proxy x) -> serialize @_ @x)
      . from
      . Record


type ADTable :: K.Rel8able -> Constraint
class
  ( Generic (Record (t Result))
  , HTable (GColumnsADT t)
  , G.GSerializeADT TSerialize TColumns (Eval (ADTRep t Expr)) (Eval (ADTRep t Result))
  ) =>
  ADTable t
instance
  ( Generic (Record (t Result))
  , HTable (GColumnsADT t)
  , G.GSerializeADT TSerialize TColumns (Eval (ADTRep t Expr)) (Eval (ADTRep t Result))
  ) =>
  ADTable t


type BuildableADT :: K.Rel8able -> Symbol -> Constraint
class GGBuildable 'K.Sum name (ADTRep t) => BuildableADT t name
instance GGBuildable 'K.Sum name (ADTRep t) => BuildableADT t name


type BuildADT :: K.Rel8able -> Symbol -> Type
type BuildADT t name = GGBuild 'K.Sum name (ADTRep t) (ADT t Expr)


buildADT :: forall t name. BuildableADT t name => BuildADT t name
buildADT =
  ggbuild @'K.Sum @name @(ADTRep t) @(ADT t Expr) ADT


type ConstructableADT :: K.Rel8able -> Constraint
class GGConstructable 'K.Sum (ADTRep t) => ConstructableADT t
instance GGConstructable 'K.Sum (ADTRep t) => ConstructableADT t


type ConstructADT :: K.Rel8able -> Type
type ConstructADT t = forall r. GGConstruct 'K.Sum (ADTRep t) r


constructADT :: forall t. ConstructableADT t => ConstructADT t -> ADT t Expr
constructADT f =
  ggconstruct @'K.Sum @(ADTRep t) @(ADT t Expr)
    ADT
    (f @(ADT t Expr))


type DeconstructADT :: K.Rel8able -> Type -> Type
type DeconstructADT t r = GGDeconstruct 'K.Sum (ADTRep t) (ADT t Expr) r


deconstructADT ::
  forall t r.
  (ConstructableADT t, Table Expr r) =>
  DeconstructADT t r
deconstructADT =
  ggdeconstruct @'K.Sum @(ADTRep t) @(ADT t Expr) @r (\(ADT a) -> a)


deconstructAADT ::
  forall t f r.
  (ConstructableADT t, Apply f, Table Expr r) =>
  DeconstructADT t (f r)
deconstructAADT =
  ggdeconstructA @'K.Sum @(ADTRep t) @(ADT t Expr) @f @r (\(ADT a) -> a)


type NameADT :: K.Rel8able -> Type
type NameADT t = GGName 'K.Sum (ADTRep t) (ADT t Name)


nameADT :: forall t. ConstructableADT t => NameADT t
nameADT = ggname @'K.Sum @(ADTRep t) @(ADT t Name) ADT


data ADTRep :: K.Rel8able -> K.Context -> Exp (Type -> Type)
type instance Eval (ADTRep t context) = GRep t context


type GColumnsADT :: K.Rel8able -> K.HTable
type GColumnsADT t = G.GColumnsADT TColumns (GRep t Expr)
