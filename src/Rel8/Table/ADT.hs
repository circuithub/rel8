{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.ADT
  ( ADT( ADT )
  , ConstructableADT
  , ConstructADT, constructADT
  , DeconstructADT, deconstructADT
  , InsertADT, insertADT
  , NameADT, nameADT
  , AggregateADT, aggregateADT
  , ADTRep
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Construction
  ( GGConstructable
  , GGConstruct, ggconstruct
  , GGDeconstruct, ggdeconstruct
  , GGInsert, gginsert
  , GGName, ggname
  , GGAggregate, ggaggregate
  )
import Rel8.Generic.Rel8able
  ( Rel8able, Algebra
  , GRep, GColumns, GContext, gfromColumns, gtoColumns
  , GColumnsADT, gfromColumnsADT, gtoColumnsADT
  , greify, gunreify
  )
import qualified Rel8.Kind.Algebra as K
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( hreify, hunreify )
import Rel8.Table ( Table )


type ADT :: K.Rel8able -> K.Rel8able
newtype ADT t context = ADT (GColumnsADT t (Col context))


instance Rel8able t => Rel8able (ADT t) where
  type Algebra (ADT t) = 'K.Product

  type GRep (ADT t) context = GRep t context

  type GColumns (ADT t) = GColumnsADT t
  type GContext (ADT t) context = context

  gfromColumns = ADT
  gtoColumns (ADT a) = a

  gfromColumnsADT = ADT
  gtoColumnsADT (ADT a) = a

  greify (ADT a) = ADT (hreify a)
  gunreify (ADT a) = ADT (hunreify a)


type ConstructableADT :: K.Rel8able -> Constraint
class GGConstructable 'K.Sum (ADTRep t) => ConstructableADT t
instance GGConstructable 'K.Sum (ADTRep t) => ConstructableADT t


type ConstructADT :: K.Rel8able -> Type
type ConstructADT t = GGConstruct 'K.Sum (ADTRep t) (ADT t Expr)


constructADT :: forall t. ConstructableADT t => ConstructADT t
constructADT = ggconstruct @'K.Sum @(ADTRep t) @(ADT t Expr) ADT


type DeconstructADT :: K.Rel8able -> Type -> Type
type DeconstructADT t r = GGDeconstruct 'K.Sum (ADTRep t) (ADT t Expr) r


deconstructADT :: forall t r. (ConstructableADT t, Table Expr r)
  => DeconstructADT t r
deconstructADT = ggdeconstruct @'K.Sum @(ADTRep t) @(ADT t Expr) @r (\(ADT a) -> a)


type InsertADT :: K.Rel8able -> Type
type InsertADT t = GGInsert 'K.Sum (ADTRep t) (ADT t Insert)


insertADT :: forall t. ConstructableADT t => InsertADT t
insertADT = gginsert @'K.Sum @(ADTRep t) @(ADT t Insert) ADT


type NameADT :: K.Rel8able -> Type
type NameADT t = GGName 'K.Sum (ADTRep t) (ADT t Name)


nameADT :: forall t. ConstructableADT t => NameADT t
nameADT = ggname @'K.Sum @(ADTRep t) @(ADT t Name) ADT


type AggregateADT :: K.Rel8able -> Type
type AggregateADT t = forall r. GGAggregate 'K.Sum (ADTRep t) r


aggregateADT :: forall t. ConstructableADT t
  => AggregateADT t -> ADT t Expr -> Aggregate (ADT t Expr)
aggregateADT f =
  ggaggregate @'K.Sum @(ADTRep t) @(ADT t Expr) ADT (\(ADT a) -> a)
    (f @(Aggregate (ADT t Expr)))


data ADTRep :: K.Rel8able -> K.Context -> Exp (Type -> Type)
type instance Eval (ADTRep t context) = GRep t context
