{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.ADT
  ( ADT( ADT )
  , ADTable
  , BuildableADT
  , BuildADT, buildADT
  , ConstructableADT
  , ConstructADT, constructADT
  , DeconstructADT, deconstructADT
  , NameADT, nameADT
  , AggregateADT, aggregateADT
  , ADTRep
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( Generic, from, to )
import GHC.TypeLits ( Symbol )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Construction
  ( GGBuildable
  , GGBuild, ggbuild
  , GGConstructable
  , GGConstruct, ggconstruct
  , GGDeconstruct, ggdeconstruct
  , GGName, ggname
  , GGAggregate, ggaggregate
  )
import Rel8.Generic.Record ( Record( Record ), unrecord )
import Rel8.Generic.Rel8able
  ( Rel8able
  , GRep, GColumns, gfromColumns, gtoColumns
  , GFromExprs, gfromResult, gtoResult
  , TSerialize, deserialize, serialize
  )
import qualified Rel8.Generic.Table.ADT as G
import qualified Rel8.Kind.Algebra as K
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )
import Rel8.Table ( Table, TColumns )


type ADT :: K.Rel8able -> K.Rel8able
newtype ADT t context = ADT (GColumnsADT t context)


instance ADTable t => Rel8able (ADT t) where
  type GColumns (ADT t) = GColumnsADT t
  type GFromExprs (ADT t) = t Result

  gfromColumns _ = ADT
  gtoColumns _ (ADT a) = a

  gfromResult =
    unrecord .
    to .
    G.gfromResultADT
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
      (\(_ :: proxy x) -> serialize @_ @x) .
    from .
    Record


type ADTable :: K.Rel8able -> Constraint
class
  ( Generic (Record (t Result))
  , HTable (GColumnsADT t)
  , G.GSerializeADT TSerialize TColumns (Eval (ADTRep t Expr)) (Eval (ADTRep t Result))
  )
  => ADTable t
instance
  ( Generic (Record (t Result))
  , HTable (GColumnsADT t)
  , G.GSerializeADT TSerialize TColumns (Eval (ADTRep t Expr)) (Eval (ADTRep t Result))
  )
  => ADTable t


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
  ggconstruct @'K.Sum @(ADTRep t) @(ADT t Expr) ADT
    (f @(ADT t Expr))


type DeconstructADT :: K.Rel8able -> Type -> Type
type DeconstructADT t r = GGDeconstruct 'K.Sum (ADTRep t) (ADT t Expr) r


deconstructADT :: forall t r. (ConstructableADT t, Table Expr r)
  => DeconstructADT t r
deconstructADT =
  ggdeconstruct @'K.Sum @(ADTRep t) @(ADT t Expr) @r (\(ADT a) -> a)


type NameADT :: K.Rel8able -> Type
type NameADT t = GGName 'K.Sum (ADTRep t) (ADT t Name)


nameADT :: forall t. ConstructableADT t => NameADT t
nameADT = ggname @'K.Sum @(ADTRep t) @(ADT t Name) ADT


type AggregateADT :: K.Rel8able -> Type
type AggregateADT t = forall r. GGAggregate 'K.Sum (ADTRep t) r


aggregateADT :: forall t. ConstructableADT t
  => AggregateADT t -> ADT t Expr -> ADT t Aggregate
aggregateADT f =
  ggaggregate @'K.Sum @(ADTRep t) @(ADT t Expr) @(ADT t Aggregate) ADT (\(ADT a) -> a)
    (f @(ADT t Aggregate))


data ADTRep :: K.Rel8able -> K.Context -> Exp (Type -> Type)
type instance Eval (ADTRep t context) = GRep t context


type GColumnsADT :: K.Rel8able -> K.HTable
type GColumnsADT t = G.GColumnsADT TColumns (GRep t Expr)
