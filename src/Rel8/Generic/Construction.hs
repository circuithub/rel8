{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

module Rel8.Generic.Construction
  ( GGConstructable
  , GGConstruct, ggconstruct
  , GGDeconstruct, ggdeconstruct
  , GGInsert, gginsert
  , GGName, ggname
  , GGAggregate, ggaggregate
  , GGAggregate', ggaggregate'
  )
where

-- base
import Data.Bifunctor ( first )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Col( Aggregation ), Aggregate )
import Rel8.Expr ( Col( DB ), Expr )
import Rel8.Expr.Aggregate ( groupByExpr )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Null ( nullify, snull, unsafeUnnullify )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.FCF ( Compose, Eval, Exp )
import Rel8.Generic.Construction.ADT
  ( GConstructableADT
  , GBuildADT, gbuildADT, gunbuildADT
  , GConstructADT, gconstructADT, gdeconstructADT
  , CorepConstructors, GConstructors, gcindex, gctabulate
  , CorepFields, gfindex, gftabulate, gftraverse
  )
import Rel8.Generic.Construction.Record
  ( GConstructable, GConstruct, gconstruct, gdeconstruct
  , Corep, gindex, gtabulate, gtraverse
  )
import Rel8.Generic.Table ( GGColumns )
import Rel8.Kind.Algebra
  ( SAlgebra( SProduct, SSum )
  , KnownAlgebra, algebraSing
  )
import qualified Rel8.Kind.Algebra as K
import Rel8.Kind.Necessity ( SNecessity( SOptional, SRequired ) )
import Rel8.Schema.Context.Nullify ( runTag )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ) )
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

-- semigroupoids
import Data.Functor.Apply ( Apply, MaybeApply( MaybeApply ), liftF2 )


type GGConstructable :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Constraint
type GGConstructable algebra rep =
  ( KnownAlgebra algebra
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Aggregate)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Expr)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Insert)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Name)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , HTable (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))))
  , GGConstructable' algebra rep
  )


type GGConstructable' :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Constraint
type family GGConstructable' algebra rep where
  GGConstructable' 'K.Product rep =
    ( Corep (Compose Aggregate TUnreify) (Eval (rep (Reify Expr)))
    , Corep TUnreify (Eval (rep (Reify Aggregate)))
    , Corep TUnreify (Eval (rep (Reify Expr)))
    , Corep TUnreify (Eval (rep (Reify Insert)))
    , Corep TUnreify (Eval (rep (Reify Name)))
    , GConstructable (TTable (Reify Aggregate)) TColumns TUnreify (Col (Reify Aggregate)) (Eval (rep (Reify Aggregate)))
    , GConstructable (TTable (Reify Expr)) TColumns TUnreify (Col (Reify Expr)) (Eval (rep (Reify Expr)))
    , GConstructable (TTable (Reify Insert)) TColumns TUnreify (Col (Reify Insert)) (Eval (rep (Reify Insert)))
    , GConstructable (TTable (Reify Name)) TColumns TUnreify (Col (Reify Name)) (Eval (rep (Reify Name)))
    )
  GGConstructable' 'K.Sum rep =
    ( CorepConstructors TUnreify (Eval (rep (Reify Expr)))
    , CorepConstructors TUnreify (Eval (rep (Reify Insert)))
    , CorepFields (Compose Aggregate TUnreify) (Eval (rep (Reify Expr)))
    , CorepFields TUnreify (Eval (rep (Reify Aggregate)))
    , CorepFields TUnreify (Eval (rep (Reify Expr)))
    , CorepFields TUnreify (Eval (rep (Reify Name)))
    , Functor (GConstructors TUnreify (Eval (rep (Reify Expr))))
    , Functor (GConstructors TUnreify (Eval (rep (Reify Insert))))
    , GConstructableADT (TTable (Reify Aggregate)) TColumns TUnreify (Col (Reify Aggregate)) (Eval (rep (Reify Aggregate)))
    , GConstructableADT (TTable (Reify Expr)) TColumns TUnreify (Col (Reify Expr)) (Eval (rep (Reify Expr)))
    , GConstructableADT (TTable (Reify Insert)) TColumns TUnreify (Col (Reify Insert)) (Eval (rep (Reify Insert)))
    , GConstructableADT (TTable (Reify Name)) TColumns TUnreify (Col (Reify Name)) (Eval (rep (Reify Name)))
    )


type GGConstruct :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGConstruct algebra rep a where
  GGConstruct 'K.Product rep a = GConstruct TUnreify (Eval (rep (Reify Expr))) a
  GGConstruct 'K.Sum rep a =
    -- Ideally this would be as follows, but GHC complains about "illegal polymorphic type"
    -- (forall r. GConstructADT TUnreify (Eval (rep (Reify Expr))) r r) -> a
    GConstructADT TUnreify (Eval (rep (Reify Expr))) a a -> a


ggconstruct :: forall algebra rep a. GGConstructable algebra rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Expr) -> a)
  -> GGConstruct algebra rep a
ggconstruct gfromColumns = case algebraSing @algebra of
  SProduct ->
    gtabulate @TUnreify @(Eval (rep (Reify Expr))) @a $
    gfromColumns .
    hunreify .
    gconstruct
      @(TTable (Reify Expr))
      @TColumns
      @TUnreify
      @(Col (Reify Expr))
      @(Eval (rep (Reify Expr)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
  SSum -> \f ->
    gcindex @TUnreify @(Eval (rep (Reify Expr))) @a f $
    fmap (gfromColumns . hunreify) $
    gconstructADT
      @(TTable (Reify Expr))
      @TColumns
      @TUnreify
      @(Col (Reify Expr))
      @(Eval (rep (Reify Expr)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
      (\SSpec {info} -> Reify (DB (snull info)))
      (\SSpec {nullity} -> case nullity of
        Null -> id
        NotNull -> \(Reify (DB a)) -> Reify (DB (nullify a)))
      (HType . Reify . DB . litExpr)


type GGDeconstruct :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type -> Type
type family GGDeconstruct algebra rep a r where
  GGDeconstruct 'K.Product rep a r =
    GConstruct TUnreify (Eval (rep (Reify Expr))) r -> a -> r
  GGDeconstruct 'K.Sum rep a r =
    GConstructADT TUnreify (Eval (rep (Reify Expr))) r (a -> r)


ggdeconstruct :: forall algebra rep a r. (GGConstructable algebra rep, Table Expr r)
  => (a -> Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Expr))
  -> GGDeconstruct algebra rep a r
ggdeconstruct gtoColumns = case algebraSing @algebra of
  SProduct -> \build ->
    gindex @TUnreify @(Eval (rep (Reify Expr))) @r build .
    gdeconstruct
      @(TTable (Reify Expr))
      @TColumns
      @TUnreify
      @(Col (Reify Expr))
      @(Eval (rep (Reify Expr)))
      (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns) .
    hreify .
    gtoColumns
  SSum ->
    gctabulate @TUnreify @(Eval (rep (Reify Expr))) @r @(a -> r) $ \constructors as ->
      let
        (HType (Reify (DB tag)), cases) =
          gdeconstructADT
            @(TTable (Reify Expr))
            @TColumns
            @TUnreify
            @(Col (Reify Expr))
            @(Eval (rep (Reify Expr)))
            (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
            (\SSpec {nullity} -> case nullity of
              Null -> id
              NotNull -> \(Reify (DB a)) -> Reify (DB (unsafeUnnullify a)))
            constructors $
          hreify $
          gtoColumns as
      in
        case cases of
          ((_, r) :| (map (first ((tag ==.) . litExpr)) -> cases')) ->
            case_ cases' r


type GGInsert :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGInsert algebra rep a where
  GGInsert 'K.Product rep a = GConstruct TUnreify (Eval (rep (Reify Insert))) a
  GGInsert 'K.Sum rep a =
    -- Ideally this would be as follows, but GHC complains about "illegal polymorphic type"
    -- (forall r. GConstructADT TUnreify (Eval (rep (Reify Insert))) r r) -> a
    GConstructADT TUnreify (Eval (rep (Reify Insert))) a a -> a


gginsert :: forall algebra rep a. GGConstructable algebra rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Insert) -> a)
  -> GGInsert algebra rep a
gginsert gfromColumns = case algebraSing @algebra of
  SProduct ->
    gtabulate @TUnreify @(Eval (rep (Reify Insert))) @a $
    gfromColumns .
    hunreify .
    gconstruct
      @(TTable (Reify Insert))
      @TColumns
      @TUnreify
      @(Col (Reify Insert))
      @(Eval (rep (Reify Insert)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
  SSum -> \f ->
    gcindex @TUnreify @(Eval (rep (Reify Insert))) @a f $
    fmap (gfromColumns . hunreify) $
    gconstructADT
      @(TTable (Reify Insert))
      @TColumns
      @TUnreify
      @(Col (Reify Insert))
      @(Eval (rep (Reify Insert)))
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


type GGName :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGName algebra rep a where
  GGName 'K.Product rep a = GConstruct TUnreify (Eval (rep (Reify Name))) a
  GGName 'K.Sum rep a = Name Tag -> GBuildADT TUnreify (Eval (rep (Reify Name))) a


ggname :: forall algebra rep a. GGConstructable algebra rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Name) -> a)
  -> GGName algebra rep a
ggname gfromColumns = case algebraSing @algebra of
  SProduct ->
    gtabulate @TUnreify @(Eval (rep (Reify Name))) @a $
    gfromColumns .
    hunreify .
    gconstruct
      @(TTable (Reify Name))
      @TColumns
      @TUnreify
      @(Col (Reify Name))
      @(Eval (rep (Reify Name)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
  SSum -> \tag ->
    gftabulate @TUnreify @(Eval (rep (Reify Name))) @a $
    gfromColumns .
    hunreify .
    gbuildADT
      @(TTable (Reify Name))
      @TColumns
      @TUnreify
      @(Col (Reify Name))
      @(Eval (rep (Reify Name)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
      (\_ _ (Reify (NameCol a)) -> Reify (NameCol a))
      (HType ((\(Name a) -> Reify (NameCol a)) tag))


type GGAggregate :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGAggregate algebra rep r where
  GGAggregate 'K.Product rep r =
    GConstruct (Compose Aggregate TUnreify) (Eval (rep (Reify Expr))) r ->
      GConstruct TUnreify (Eval (rep (Reify Expr))) r
  GGAggregate 'K.Sum rep r =
    GBuildADT (Compose Aggregate TUnreify) (Eval (rep (Reify Expr))) r ->
      GBuildADT TUnreify (Eval (rep (Reify Expr))) r


ggaggregate :: forall algebra rep a. GGConstructable algebra rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Expr) -> a)
  -> (a -> Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Expr))
  -> GGAggregate algebra rep (Aggregate a) -> a -> Aggregate a
ggaggregate gfromColumns gtoColumns agg es = case algebraSing @algebra of
  SProduct -> flip f exprs $
    fmap
      ( gfromColumns
      . hunreify
      . gconstruct
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
      ) .
    liftF½ .
    gtraverse @TUnreify @(Eval (rep (Reify Expr))) @_ @Aggregate (MaybeApply . Left)
    where
      f = 
        gindex @TUnreify @(Eval (rep (Reify Expr))) @(Aggregate a) .
        agg .
        gtabulate @(Compose Aggregate TUnreify) @(Eval (rep (Reify Expr))) @(Aggregate a)
      exprs =
        gdeconstruct
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns) $
        hreify $
        gtoColumns es
  SSum -> flip f exprs $
    liftF1½
      (\tag' ->
        gfromColumns .
        hunreify .
        gbuildADT
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
          (\tag'' SSpec {nullity} (Reify (DB a)) ->
            Reify $ DB $ runTag nullity (tag ==. litExpr tag'') a)
          (HType (Reify (DB tag'))))
      (groupByExpr tag) .
    gftraverse @TUnreify @(Eval (rep (Reify Expr))) @_ @Aggregate (MaybeApply . Left)
    where
      f =
        gfindex @TUnreify @(Eval (rep (Reify Expr))) @(Aggregate a) .
        agg .
        gftabulate @(Compose Aggregate TUnreify) @(Eval (rep (Reify Expr))) @(Aggregate a)
      (HType (Reify (DB tag)), exprs) =
        gunbuildADT
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
          (\SSpec {nullity} -> case nullity of
            Null -> id
            NotNull -> \(Reify (DB a)) -> Reify (DB (unsafeUnnullify a))) $
        hreify $
        gtoColumns es


type GGAggregate' :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGAggregate' algebra rep r where
  GGAggregate' 'K.Product rep r =
    GConstruct TUnreify (Eval (rep (Reify Aggregate))) r ->
      GConstruct TUnreify (Eval (rep (Reify Expr))) r
  GGAggregate' 'K.Sum rep r =
    GBuildADT TUnreify (Eval (rep (Reify Aggregate))) r ->
      GBuildADT TUnreify (Eval (rep (Reify Expr))) r


ggaggregate' :: forall algebra rep exprs agg. GGConstructable algebra rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Aggregate) -> agg)
  -> (exprs -> Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Expr))
  -> GGAggregate' algebra rep agg -> exprs -> agg
ggaggregate' gfromColumns gtoColumns agg es = case algebraSing @algebra of
  SProduct -> flip f exprs $
    gfromColumns .
    hunreify .
    gconstruct
      @(TTable (Reify Aggregate))
      @TColumns
      @TUnreify
      @(Col (Reify Aggregate))
      @(Eval (rep (Reify Aggregate)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
    where
      f =
        gindex @TUnreify @(Eval (rep (Reify Expr))) @agg .
        agg .
        gtabulate @TUnreify @(Eval (rep (Reify Aggregate))) @agg
      exprs =
        gdeconstruct
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns) $
        hreify $
        gtoColumns es
  SSum -> flip f exprs $
    gfromColumns .
    hunreify .
    gbuildADT
      @(TTable (Reify Aggregate))
      @TColumns
      @TUnreify
      @(Col (Reify Aggregate))
      @(Eval (rep (Reify Aggregate)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
      (\tag' SSpec {nullity} (Reify (Aggregation a)) ->
        Reify $ Aggregation $ runTag nullity (tag ==. litExpr tag') <$> a)
      (HType (Reify (Aggregation (groupByExpr tag))))
    where
      f =
        gfindex @TUnreify @(Eval (rep (Reify Expr))) @agg .
        agg .
        gftabulate @TUnreify @(Eval (rep (Reify Aggregate))) @agg
      (HType (Reify (DB tag)), exprs) =
        gunbuildADT
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
          (\SSpec {nullity} -> case nullity of
            Null -> id
            NotNull -> \(Reify (DB a)) -> Reify (DB (unsafeUnnullify a))) $
        hreify $
        gtoColumns es


liftF½ :: MaybeApply Aggregate a -> Aggregate a
liftF½ (MaybeApply ma) = case ma of
  Left a -> a
  Right a -> a <$ groupByExpr (litExpr False)


liftF1½ :: Apply v => (a -> b -> c) -> v a -> MaybeApply v b -> v c
liftF1½ f a (MaybeApply eb) = case eb of
  Left b -> liftF2 f a b
  Right b -> fmap (`f` b) a
