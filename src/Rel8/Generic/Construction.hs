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
  ( GGBuildable
  , GGBuild, ggbuild
  , GGInsert, gginsert
  , GGConstructable
  , GGConstruct, ggconstruct
  , GGDeconstruct, ggdeconstruct
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
import GHC.TypeLits ( Symbol )
import Prelude

-- rel8
import Rel8.Aggregate ( Col( A ), Aggregate )
import Rel8.Expr ( Col( E ), Expr )
import Rel8.Expr.Aggregate ( groupByExpr )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Null ( nullify, snull, unsafeUnnullify )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.FCF ( Compose, Eval, Exp )
import Rel8.Generic.Construction.ADT
  ( GConstructorADT, GMakeableADT, gmakeADT
  , GConstructableADT
  , GBuildADT, gbuildADT, gunbuildADT
  , GConstructADT, gconstructADT, gdeconstructADT
  , CorepConstructors, GConstructors, gcindex, gctabulate
  , CorepFields, gfindex, gftabulate, gftraverse
  )
import Rel8.Generic.Construction.Record
  ( GConstructor
  , GConstructable, GConstruct, gconstruct, gdeconstruct
  , Corep, gindex, gtabulate, gtraverse
  )
import Rel8.Generic.Table ( GGColumns )
import Rel8.Kind.Algebra
  ( SAlgebra( SProduct, SSum )
  , KnownAlgebra, algebraSing
  )
import qualified Rel8.Kind.Algebra as K
import Rel8.Schema.Context.Nullify ( runTag )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ) )
import Rel8.Schema.Insert ( Col( I ), Insert, Insertion(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Col( N ), Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( SSpec( SSpec, nullity, info ) )
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


type GGBuildable :: K.Algebra -> Symbol -> (K.Context -> Exp (Type -> Type)) -> Constraint
type GGBuildable algebra name rep =
  ( KnownAlgebra algebra
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Aggregate)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Expr)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Insert)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , Eval (GGColumns algebra TColumns (Eval (rep (Reify Name)))) ~ Eval (GGColumns algebra TColumns (Eval (rep (Reify Result))))
  , HTable (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))))
  , GGBuildable' algebra name rep
  )


type GGBuildable' :: K.Algebra -> Symbol -> (K.Context -> Exp (Type -> Type)) -> Constraint
type family GGBuildable' algebra name rep where
  GGBuildable' 'K.Product name rep =
    ( name ~ GConstructor (Eval (rep (Reify Expr)))
    , name ~ GConstructor (Eval (rep (Reify Insert)))
    , Corep TUnreify (Eval (rep (Reify Expr)))
    , Corep TUnreify (Eval (rep (Reify Insert)))
    , GConstructable (TTable (Reify Expr)) TColumns TUnreify (Col (Reify Expr)) (Eval (rep (Reify Expr)))
    , GConstructable (TTable (Reify Insert)) TColumns TUnreify (Col (Reify Insert)) (Eval (rep (Reify Insert)))
    )
  GGBuildable' 'K.Sum name rep =
    ( Corep TUnreify (GConstructorADT name (Eval (rep (Reify Expr))))
    , Corep TUnreify (GConstructorADT name (Eval (rep (Reify Insert))))
    , GMakeableADT (TTable (Reify Expr)) TColumns TUnreify (Col (Reify Expr)) name (Eval (rep (Reify Expr)))
    , GMakeableADT (TTable (Reify Insert)) TColumns TUnreify (Col (Reify Insert)) name (Eval (rep (Reify Insert)))
    )


type GGBuild :: K.Algebra -> Symbol -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGBuild algebra name rep r where
  GGBuild 'K.Product _name rep r =
    GConstruct TUnreify (Eval (rep (Reify Expr))) r
  GGBuild 'K.Sum name rep r =
    GConstruct TUnreify (GConstructorADT name (Eval (rep (Reify Expr)))) r


ggbuild :: forall algebra name rep a. GGBuildable algebra name rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Expr) -> a)
  -> GGBuild algebra name rep a
ggbuild gfromColumns = case algebraSing @algebra of
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
  SSum ->
    gtabulate @TUnreify @(GConstructorADT name (Eval (rep (Reify Expr)))) @a $
    gfromColumns .
    hunreify .
    gmakeADT
      @(TTable (Reify Expr))
      @TColumns
      @TUnreify
      @(Col (Reify Expr))
      @name
      @(Eval (rep (Reify Expr)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
      (\SSpec {info} -> Reify (E (snull info)))
      (\SSpec {nullity} -> case nullity of
        Null -> id
        NotNull -> \(Reify (E a)) -> Reify (E (nullify a)))
      (HType . Reify . E . litExpr)


type GGInsert :: K.Algebra -> Symbol -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGInsert algebra name rep r where
  GGInsert 'K.Product _name rep r =
    GConstruct TUnreify (Eval (rep (Reify Insert))) r
  GGInsert 'K.Sum name rep r =
    GConstruct TUnreify (GConstructorADT name (Eval (rep (Reify Insert)))) r


gginsert :: forall algebra name rep a. GGBuildable algebra name rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Insert) -> a)
  -> GGInsert algebra name rep a
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
  SSum ->
    gtabulate @TUnreify @(GConstructorADT name (Eval (rep (Reify Insert)))) @a $
    gfromColumns .
    hunreify .
    gmakeADT
      @(TTable (Reify Insert))
      @TColumns
      @TUnreify
      @(Col (Reify Insert))
      @name
      @(Eval (rep (Reify Insert)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
      (\SSpec {info} -> Reify $ I (Value (snull info)))
      (\SSpec {nullity} -> case nullity of
        Null -> id
        NotNull -> \case
          Reify (I Default) -> Reify (I Default)
          Reify (I (Value a)) -> Reify (I (Value (nullify a))))
      (HType . Reify . I . Value . litExpr)


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
type family GGConstruct algebra rep r where
  GGConstruct 'K.Product rep r =
    GConstruct TUnreify (Eval (rep (Reify Expr))) r -> r
  GGConstruct 'K.Sum rep r =
    GConstructADT TUnreify (Eval (rep (Reify Expr))) r r


ggconstruct :: forall algebra rep a. GGConstructable algebra rep
  => (Eval (GGColumns algebra TColumns (Eval (rep (Reify Result)))) (Col Expr) -> a)
  -> GGConstruct algebra rep a -> a
ggconstruct gfromColumns f = case algebraSing @algebra of
  SProduct ->
    f $
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
  SSum ->
    gcindex @TUnreify @(Eval (rep (Reify Expr))) @a f $
    fmap (gfromColumns . hunreify) $
    gconstructADT
      @(TTable (Reify Expr))
      @TColumns
      @TUnreify
      @(Col (Reify Expr))
      @(Eval (rep (Reify Expr)))
      (\(_ :: proxy x) -> toColumns . reify @_ @x Refl)
      (\SSpec {info} -> Reify (E (snull info)))
      (\SSpec {nullity} -> case nullity of
        Null -> id
        NotNull -> \(Reify (E a)) -> Reify (E (nullify a)))
      (HType . Reify . E . litExpr)


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
        (HType (Reify (E tag)), cases) =
          gdeconstructADT
            @(TTable (Reify Expr))
            @TColumns
            @TUnreify
            @(Col (Reify Expr))
            @(Eval (rep (Reify Expr)))
            (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
            (\SSpec {nullity} -> case nullity of
              Null -> id
              NotNull -> \(Reify (E a)) -> Reify (E (unsafeUnnullify a)))
            constructors $
          hreify $
          gtoColumns as
      in
        case cases of
          ((_, r) :| (map (first ((tag ==.) . litExpr)) -> cases')) ->
            case_ cases' r


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
      (\_ _ (Reify (N (Name a))) -> Reify (N (Name a)))
      (HType (Reify (N tag)))


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
          (\tag'' SSpec {nullity} (Reify (E a)) ->
            Reify $ E $ runTag nullity (tag ==. litExpr tag'') a)
          (HType (Reify (E tag'))))
      (groupByExpr tag) .
    gftraverse @TUnreify @(Eval (rep (Reify Expr))) @_ @Aggregate (MaybeApply . Left)
    where
      f =
        gfindex @TUnreify @(Eval (rep (Reify Expr))) @(Aggregate a) .
        agg .
        gftabulate @(Compose Aggregate TUnreify) @(Eval (rep (Reify Expr))) @(Aggregate a)
      (HType (Reify (E tag)), exprs) =
        gunbuildADT
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
          (\SSpec {nullity} -> case nullity of
            Null -> id
            NotNull -> \(Reify (E a)) -> Reify (E (unsafeUnnullify a))) $
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
      (\tag' SSpec {nullity} (Reify (A a)) ->
        Reify $ A $ runTag nullity (tag ==. litExpr tag') <$> a)
      (HType (Reify (A (groupByExpr tag))))
    where
      f =
        gfindex @TUnreify @(Eval (rep (Reify Expr))) @agg .
        agg .
        gftabulate @TUnreify @(Eval (rep (Reify Aggregate))) @agg
      (HType (Reify (E tag)), exprs) =
        gunbuildADT
          @(TTable (Reify Expr))
          @TColumns
          @TUnreify
          @(Col (Reify Expr))
          @(Eval (rep (Reify Expr)))
          (\(_ :: proxy x) -> unreify @_ @x Refl . fromColumns)
          (\SSpec {nullity} -> case nullity of
            Null -> id
            NotNull -> \(Reify (E a)) -> Reify (E (unsafeUnnullify a))) $
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
