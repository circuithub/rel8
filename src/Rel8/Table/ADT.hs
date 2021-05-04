{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

module Rel8.Table.ADT
  ( ADT( ADT )
  , ConstructableADT
  , ConstructADT, constructADT
  , DeconstructADT, deconstructADT
  , InsertADT, insertADT
  , NameADT, nameADT
  , AggregateADT, aggregateADT
  )
where

-- base
import Data.Bifunctor ( first )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Col( Aggregation ), Aggregate, mapInputs )
import Rel8.Expr ( Col( DB ), Expr )
import Rel8.Expr.Aggregate ( groupByExpr )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Null ( nullify, snull, unsafeUnnullify )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Generic.Construction.ADT
  ( GConstructableADT
  , GBuildADT, gbuildADT, gunbuildADT
  , GConstructADT, gconstructADT, gdeconstructADT
  , CorepConstructors, GConstructors, gcindex, gctabulate
  , CorepFields, GFields, gfindex, gftabulate
  )
import Rel8.Generic.Rel8able
  ( Rel8able, Algebra
  , GRep, GColumns, GContext, gfromColumns, gtoColumns
  , GColumnsADT, gfromColumnsADT, gtoColumnsADT
  , greify, gunreify
  )
import qualified Rel8.Generic.Table.ADT as G
import qualified Rel8.Kind.Algebra as K
import Rel8.Kind.Necessity ( SNecessity( SOptional, SRequired ) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Nullify ( runTag )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import Rel8.Schema.Insert ( Col( OptionalInsert, RequiredInsert ), Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Col( NameCol ), Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( SSpec( SSpec, necessity, nullity, info ) )
import Rel8.Schema.Reify ( Col( Reify ), Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( TTable, TColumns, TUnreify
  , Table, fromColumns, toColumns, reify, unreify
  )
import Rel8.Table.Bool ( case_ )
import Rel8.Type.Tag ( Tag )


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
class
  ( CorepConstructors TUnreify (GRep t (Reify Expr))
  , CorepConstructors TUnreify (GRep t (Reify Insert))
  , CorepFields TUnreify (GRep t (Reify Aggregate))
  , CorepFields TUnreify (GRep t (Reify Expr))
  , CorepFields TUnreify (GRep t (Reify Name))
  , G.GColumnsADT TColumns (GRep t (Reify Aggregate)) ~ GColumnsADT t
  , G.GColumnsADT TColumns (GRep t (Reify Expr)) ~ GColumnsADT t
  , G.GColumnsADT TColumns (GRep t (Reify Insert)) ~ GColumnsADT t
  , G.GColumnsADT TColumns (GRep t (Reify Name)) ~ GColumnsADT t
  , Functor (GConstructors TUnreify (GRep t (Reify Expr)))
  , Functor (GConstructors TUnreify (GRep t (Reify Insert)))
  , HTable (G.GColumnsADT TColumns (GRep t (Reify Result)))
  , GConstructableADT (TTable (Reify Aggregate)) TColumns TUnreify (Col (Reify Aggregate)) (GRep t (Reify Aggregate))
  , GConstructableADT (TTable (Reify Expr)) TColumns TUnreify (Col (Reify Expr)) (GRep t (Reify Expr))
  , GConstructableADT (TTable (Reify Insert)) TColumns TUnreify (Col (Reify Insert)) (GRep t (Reify Insert))
  , GConstructableADT (TTable (Reify Name)) TColumns TUnreify (Col (Reify Name)) (GRep t (Reify Name))
  )
  => ConstructableADT t
instance
  ( CorepConstructors TUnreify (GRep t (Reify Expr))
  , CorepConstructors TUnreify (GRep t (Reify Insert))
  , CorepFields TUnreify (GRep t (Reify Aggregate))
  , CorepFields TUnreify (GRep t (Reify Expr))
  , CorepFields TUnreify (GRep t (Reify Name))
  , G.GColumnsADT TColumns (GRep t (Reify Aggregate)) ~ GColumnsADT t
  , G.GColumnsADT TColumns (GRep t (Reify Expr)) ~ GColumnsADT t
  , G.GColumnsADT TColumns (GRep t (Reify Insert)) ~ GColumnsADT t
  , G.GColumnsADT TColumns (GRep t (Reify Name)) ~ GColumnsADT t
  , Functor (GConstructors TUnreify (GRep t (Reify Expr)))
  , Functor (GConstructors TUnreify (GRep t (Reify Insert)))
  , HTable (G.GColumnsADT TColumns (GRep t (Reify Result)))
  , GConstructableADT (TTable (Reify Aggregate)) TColumns TUnreify (Col (Reify Aggregate)) (GRep t (Reify Aggregate))
  , GConstructableADT (TTable (Reify Expr)) TColumns TUnreify (Col (Reify Expr)) (GRep t (Reify Expr))
  , GConstructableADT (TTable (Reify Insert)) TColumns TUnreify (Col (Reify Insert)) (GRep t (Reify Insert))
  , GConstructableADT (TTable (Reify Name)) TColumns TUnreify (Col (Reify Name)) (GRep t (Reify Name))
  )
  => ConstructableADT t


type ConstructADT :: K.Rel8able -> Type
type ConstructADT t = (forall r. GConstructADT TUnreify (GRep t (Reify Expr)) r r) -> ADT t Expr


constructADT :: forall t. ConstructableADT t => ConstructADT t
constructADT f =
  gcindex @TUnreify @(GRep t (Reify Expr)) @(ADT t Expr) (f @(ADT t Expr)) $
  fmap (ADT . hunreify) $
  gconstructADT
    @(TTable (Reify Expr))
    @TColumns
    @TUnreify
    @(Col (Reify Expr))
    @(GRep t (Reify Expr))
    (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
    (\SSpec {info} -> Reify (DB (snull info)))
    (\SSpec {nullity} -> case nullity of
      Null -> id
      NotNull -> \(Reify (DB a)) -> Reify (DB (nullify a)))
    (HType . Reify . DB . litExpr)


type DeconstructADT :: K.Rel8able -> Type -> Type
type DeconstructADT t r = GConstructADT TUnreify (GRep t (Reify Expr)) r (ADT t Expr -> r)


deconstructADT :: forall t r. (ConstructableADT t, Table Expr r)
  => DeconstructADT t r
deconstructADT =
  gctabulate @TUnreify @(GRep t (Reify Expr)) @r @(ADT t Expr -> r)
  \constructors (ADT columns) ->
    let
      (HType (Reify (DB tag)), cases) = gdeconstructADT
        @(TTable (Reify Expr))
        @TColumns
        @TUnreify
        @(Col (Reify Expr))
        @(GRep t (Reify Expr))
        (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
        (\SSpec {nullity} -> case nullity of
          Null -> id
          NotNull -> \(Reify (DB a)) -> Reify (DB (unsafeUnnullify a)))
        constructors
        (hreify columns)
    in
      case cases of
        ((_, r) :| (map (first ((tag ==.) . litExpr)) -> cases')) ->
          case_ cases' r


type InsertADT :: K.Rel8able -> Type
type InsertADT t = (forall r. GConstructADT TUnreify (GRep t (Reify Insert)) r r) -> ADT t Insert


insertADT :: forall t. ConstructableADT t => InsertADT t
insertADT f =
  gcindex @TUnreify @(GRep t (Reify Insert)) @(ADT t Insert) (f @(ADT t Insert)) $
  fmap (ADT . hunreify) $
  gconstructADT
    @(TTable (Reify Insert))
    @TColumns
    @TUnreify
    @(Col (Reify Insert))
    @(GRep t (Reify Insert))
    (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
    (\SSpec {necessity, info} -> Reify $ case necessity of
      SRequired -> RequiredInsert (snull info)
      SOptional -> OptionalInsert (Just (snull info)))
    (\SSpec {nullity} -> case nullity of
      Null -> id
      NotNull -> \case
        Reify (RequiredInsert a) -> Reify (RequiredInsert (nullify a))
        Reify (OptionalInsert a) -> Reify (OptionalInsert (nullify <$> a)))
    (HType . Reify . RequiredInsert . litExpr)


type NameADT :: K.Rel8able -> Type
type NameADT t = Name Tag -> GBuildADT TUnreify (GRep t (Reify Name)) (ADT t Name)


nameADT :: forall t. ConstructableADT t => NameADT t
nameADT tag = gftabulate @TUnreify @(GRep t (Reify Name)) @(ADT t Name) $
  ADT .
  hunreify .
  gbuildADT
    @(TTable (Reify Name))
    @TColumns
    @TUnreify
    @(Col (Reify Name))
    @(GRep t (Reify Name))
    (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
    (\_ _ (Reify (NameCol a)) -> Reify (NameCol a))
    (HType ((\(Name a) -> Reify (NameCol a)) tag))


type AggregateADT :: K.Rel8able -> Type
type AggregateADT t = forall r.
  GBuildADT TUnreify (GRep t (Reify Expr))
    (GBuildADT TUnreify (GRep t (Reify Aggregate)) r -> r)


aggregateADT :: forall t. ConstructableADT t
  => AggregateADT t -> ADT t Expr -> ADT t Aggregate
aggregateADT builder (ADT columns) = f exprs $
  ADT .
  hunreify .
  gbuildADT
    @(TTable (Reify Aggregate))
    @TColumns
    @TUnreify
    @(Col (Reify Aggregate))
    @(GRep t (Reify Aggregate))
    (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
    (\tag' SSpec {nullity} (Reify (Aggregation aggregate)) ->
      let
        condition = tag ==. litExpr tag'
      in
        Reify $
        Aggregation $
        mapInputs (toPrimExpr . runTag nullity condition . fromPrimExpr) $
        runTag nullity condition <$> aggregate)
    (HType (Reify (Aggregation (groupByExpr tag))))
  where
    f e =
      gfindex
        @TUnreify
        @(GRep t (Reify Expr))
        @(GBuildADT TUnreify (GRep t (Reify Aggregate)) (ADT t Aggregate) -> ADT t Aggregate)
        (builder @(ADT t Aggregate))
        e .
      gftabulate
        @TUnreify
        @(GRep t (Reify Aggregate))
        @(ADT t Aggregate)
    (HType (Reify (DB tag)), exprs) =
      gunbuildADT
        @(TTable (Reify Expr))
        @TColumns
        @TUnreify
        @(Col (Reify Expr))
        @(GRep t (Reify Expr))
        (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
        (\SSpec {nullity} -> case nullity of
          Null -> id
          NotNull -> \(Reify (DB a)) -> Reify (DB (unsafeUnnullify a)))
        (hreify columns)
