{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.ADT
  ( ADT( ADT )
  , ADTable, fromADT, toADT
  , BuildableADT
  , BuildADT, buildADT
  , InsertADT, insertADT
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
import Data.Proxy ( Proxy( Proxy ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import GHC.Generics ( Generic, Rep, from, to )
import GHC.TypeLits ( Symbol )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Construction
  ( GGBuildable
  , GGBuild, ggbuild
  , GGInsert, gginsert
  , GGConstructable
  , GGConstruct, ggconstruct
  , GGDeconstruct, ggdeconstruct
  , GGName, ggname
  , GGAggregate, ggaggregate
  )
import Rel8.Generic.Map ( GMappable, GMap, gmap, gunmap )
import Rel8.Generic.Record ( GRecordable, GRecord, grecord, gunrecord )
import Rel8.Generic.Rel8able
  ( Rel8able
  , GRep, GColumns, gfromColumns, gtoColumns
  , greify, gunreify
  )
import qualified Rel8.Generic.Table.ADT as G
import qualified Rel8.Kind.Algebra as K
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Col( Reify ), Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table
  , fromColumns, toColumns, reify, unreify
  , TTable, TColumns, TUnreify
  )


type ADT :: K.Rel8able -> K.Rel8able
newtype ADT t context = ADT (GColumnsADT t (Col context))


instance ADTable t => Rel8able (ADT t) where
  type GColumns (ADT t) = GColumnsADT t

  gfromColumns = ADT
  gtoColumns (ADT a) = a

  greify (ADT a) = ADT (hreify a)
  gunreify (ADT a) = ADT (hunreify a)


instance (ADTable t, context ~ Result) => Generic (ADT t context) where
  type Rep (ADT t context) = Rep (t context)

  from =
    gmap @(TTable (Reify Result)) (Proxy @TUnreify) (unreify Refl) .
    gunrecord @(Rep (t (Reify Result))) .
    G.gfromColumnsADT
      @(TTable (Reify Result))
      @TColumns
      (\(Reify a) -> a)
      Reify
      fromColumns .
    hreify .
    (\(ADT a) -> a)

  to =
    ADT .
    hunreify .
    G.gtoColumnsADT
      @(TTable (Reify Result))
      @TColumns
      (\(Reify a) -> a)
      Reify
      toColumns .
    grecord @(Rep (t (Reify Result))) .
    gunmap @(TTable (Reify Result)) (Proxy @TUnreify) (reify Refl)


fromADT :: ADTable t => ADT t Result -> t Result
fromADT = to . from


toADT :: ADTable t => t Result -> ADT t Result
toADT = to . from


type ADTable :: K.Rel8able -> Constraint
class
  ( Generic (t Result)
  , HTable (GColumnsADT t)
  , G.GTableADT (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (Rep (t (Reify Result))))
  , GRecordable (Rep (t (Reify Result)))
  , GMappable (TTable (Reify Result)) (Rep (t (Reify Result)))
  , GMap TUnreify (Rep (t (Reify Result))) ~ Rep (t Result)
  )
  => ADTable t
instance
  ( Generic (t Result)
  , HTable (GColumnsADT t)
  , G.GTableADT (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (Rep (t (Reify Result))))
  , GRecordable (Rep (t (Reify Result)))
  , GMappable (TTable (Reify Result)) (Rep (t (Reify Result)))
  , GMap TUnreify (Rep (t (Reify Result))) ~ Rep (t Result)
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


type InsertADT :: K.Rel8able -> Symbol -> Type
type InsertADT t name = GGInsert 'K.Sum name (ADTRep t) (ADT t Insert)


insertADT :: forall t name. BuildableADT t name => InsertADT t name
insertADT =
  gginsert @'K.Sum @name @(ADTRep t) @(ADT t Insert) ADT


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
type GColumnsADT t = G.GColumnsADT TColumns (GRep t (Reify Result))
