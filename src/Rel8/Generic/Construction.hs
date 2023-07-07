{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Rel8.Generic.Construction (
  GGBuildable,
  GGBuild,
  ggbuild,
  GGConstructable,
  GGConstruct,
  ggconstruct,
  GGDeconstruct,
  ggdeconstruct,
  ggdeconstructA,
  GGName,
  ggname,
)
where

-- base
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.TypeLits (Symbol)
import Prelude

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Eq ((==.))
import Rel8.Expr.Null (nullify, snull, unsafeUnnullify)
import Rel8.Expr.Serialize (litExpr)
import Rel8.FCF (Eval, Exp, Id)
import Rel8.Generic.Construction.ADT (
  GBuildADT,
  GConstructADT,
  GConstructableADT,
  GConstructorADT,
  GConstructors,
  GMakeableADT,
  RepresentableConstructors,
  RepresentableFields,
  gbuildADT,
  gcindex,
  gconstructADT,
  gctabulate,
  gdeconstructADT,
  gftabulate,
  gmakeADT,
 )
import Rel8.Generic.Construction.Record (
  GConstruct,
  GConstructable,
  GConstructor,
  Representable,
  gconstruct,
  gdeconstruct,
  gindex,
  gtabulate,
 )
import Rel8.Generic.Table (GGColumns)
import Rel8.Kind.Algebra (
  KnownAlgebra,
  SAlgebra (SProduct, SSum),
  algebraSing,
 )
import qualified Rel8.Kind.Algebra as K
import Rel8.Schema.HTable (HTable)
import Rel8.Schema.HTable.Identity (HIdentity (HIdentity))
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name (Name (Name))
import Rel8.Schema.Null (Nullity (NotNull, Null))
import Rel8.Schema.Spec (Spec (Spec, info, nullity))
import Rel8.Table (
  TColumns,
  TTable,
  Table,
  fromColumns,
  toColumns,
 )
import Rel8.Table.Bool (case_)
import Rel8.Type.Tag (Tag)

-- semigroupoids
import Data.Functor.Apply (Apply)
import Data.Semigroup.Traversable (sequence1, traverse1)


type GGBuildable :: K.Algebra -> Symbol -> (K.Context -> Exp (Type -> Type)) -> Constraint
type GGBuildable algebra name rep =
  ( KnownAlgebra algebra
  , Eval (GGColumns algebra TColumns (Eval (rep Expr))) ~ Eval (GGColumns algebra TColumns (Eval (rep Expr)))
  , Eval (GGColumns algebra TColumns (Eval (rep Name))) ~ Eval (GGColumns algebra TColumns (Eval (rep Expr)))
  , HTable (Eval (GGColumns algebra TColumns (Eval (rep Expr))))
  , GGBuildable' algebra name rep
  )


type GGBuildable' :: K.Algebra -> Symbol -> (K.Context -> Exp (Type -> Type)) -> Constraint
type family GGBuildable' algebra name rep where
  GGBuildable' 'K.Product name rep =
    ( name ~ GConstructor (Eval (rep Expr))
    , Representable Id (Eval (rep Expr))
    , GConstructable (TTable Expr) TColumns Id Expr (Eval (rep Expr))
    )
  GGBuildable' 'K.Sum name rep =
    ( Representable Id (GConstructorADT name (Eval (rep Expr)))
    , GMakeableADT (TTable Expr) TColumns Id Expr name (Eval (rep Expr))
    )


type GGBuild :: K.Algebra -> Symbol -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGBuild algebra name rep r where
  GGBuild 'K.Product _name rep r =
    GConstruct Id (Eval (rep Expr)) r
  GGBuild 'K.Sum name rep r =
    GConstruct Id (GConstructorADT name (Eval (rep Expr))) r


ggbuild ::
  forall algebra name rep a.
  GGBuildable algebra name rep =>
  (Eval (GGColumns algebra TColumns (Eval (rep Expr))) Expr -> a) ->
  GGBuild algebra name rep a
ggbuild gfromColumns = case algebraSing @algebra of
  SProduct ->
    gtabulate @Id @(Eval (rep Expr)) @a $
      gfromColumns
        . gconstruct
          @(TTable Expr)
          @TColumns
          @Id
          @Expr
          @(Eval (rep Expr))
          (const toColumns)
  SSum ->
    gtabulate @Id @(GConstructorADT name (Eval (rep Expr))) @a $
      gfromColumns
        . gmakeADT
          @(TTable Expr)
          @TColumns
          @Id
          @Expr
          @name
          @(Eval (rep Expr))
          (const toColumns)
          (\Spec{info} -> snull info)
          ( \Spec{nullity} -> case nullity of
              Null -> id
              NotNull -> nullify
          )
          (HIdentity . litExpr)


type GGConstructable :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Constraint
type GGConstructable algebra rep =
  ( KnownAlgebra algebra
  , Eval (GGColumns algebra TColumns (Eval (rep Expr))) ~ Eval (GGColumns algebra TColumns (Eval (rep Expr)))
  , Eval (GGColumns algebra TColumns (Eval (rep Name))) ~ Eval (GGColumns algebra TColumns (Eval (rep Expr)))
  , HTable (Eval (GGColumns algebra TColumns (Eval (rep Expr))))
  , GGConstructable' algebra rep
  )


type GGConstructable' :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Constraint
type family GGConstructable' algebra rep where
  GGConstructable' 'K.Product rep =
    ( Representable Id (Eval (rep Expr))
    , Representable Id (Eval (rep Name))
    , GConstructable (TTable Expr) TColumns Id Expr (Eval (rep Expr))
    , GConstructable (TTable Name) TColumns Id Name (Eval (rep Name))
    )
  GGConstructable' 'K.Sum rep =
    ( RepresentableConstructors Id (Eval (rep Expr))
    , RepresentableFields Id (Eval (rep Expr))
    , RepresentableFields Id (Eval (rep Name))
    , Functor (GConstructors Id (Eval (rep Expr)))
    , GConstructableADT (TTable Expr) TColumns Id Expr (Eval (rep Expr))
    , GConstructableADT (TTable Name) TColumns Id Name (Eval (rep Name))
    )


type GGConstruct :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGConstruct algebra rep r where
  GGConstruct 'K.Product rep r = GConstruct Id (Eval (rep Expr)) r -> r
  GGConstruct 'K.Sum rep r = GConstructADT Id (Eval (rep Expr)) r r


ggconstruct ::
  forall algebra rep a.
  GGConstructable algebra rep =>
  (Eval (GGColumns algebra TColumns (Eval (rep Expr))) Expr -> a) ->
  GGConstruct algebra rep a ->
  a
ggconstruct gfromColumns f = case algebraSing @algebra of
  SProduct ->
    f $
      gtabulate @Id @(Eval (rep Expr)) @a $
        gfromColumns
          . gconstruct
            @(TTable Expr)
            @TColumns
            @Id
            @Expr
            @(Eval (rep Expr))
            (const toColumns)
  SSum ->
    gcindex @Id @(Eval (rep Expr)) @a f $
      fmap gfromColumns $
        gconstructADT
          @(TTable Expr)
          @TColumns
          @Id
          @Expr
          @(Eval (rep Expr))
          (const toColumns)
          (\Spec{info} -> snull info)
          ( \Spec{nullity} -> case nullity of
              Null -> id
              NotNull -> nullify
          )
          (HIdentity . litExpr)


type GGDeconstruct :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type -> Type
type family GGDeconstruct algebra rep a r where
  GGDeconstruct 'K.Product rep a r =
    GConstruct Id (Eval (rep Expr)) r -> a -> r
  GGDeconstruct 'K.Sum rep a r =
    GConstructADT Id (Eval (rep Expr)) r (a -> r)


ggdeconstruct ::
  forall algebra rep a r.
  (GGConstructable algebra rep, Table Expr r) =>
  (a -> Eval (GGColumns algebra TColumns (Eval (rep Expr))) Expr) ->
  GGDeconstruct algebra rep a r
ggdeconstruct gtoColumns = case algebraSing @algebra of
  SProduct -> \build ->
    gindex @Id @(Eval (rep Expr)) @r build
      . gdeconstruct
        @(TTable Expr)
        @TColumns
        @Id
        @Expr
        @(Eval (rep Expr))
        (const fromColumns)
      . gtoColumns
  SSum ->
    gctabulate @Id @(Eval (rep Expr)) @r @(a -> r) $ \constructors as ->
      let
        (HIdentity tag, cases) =
          gdeconstructADT
            @(TTable Expr)
            @TColumns
            @Id
            @Expr
            @(Eval (rep Expr))
            (const fromColumns)
            ( \Spec{nullity} -> case nullity of
                Null -> id
                NotNull -> unsafeUnnullify
            )
            constructors
            $ gtoColumns as
       in
        case cases of
          ((_, r) :| (map (first ((tag ==.) . litExpr)) -> cases')) ->
            case_ cases' r


ggdeconstructA ::
  forall algebra rep a f r.
  (GGConstructable algebra rep, Apply f, Table Expr r) =>
  (a -> Eval (GGColumns algebra TColumns (Eval (rep Expr))) Expr) ->
  GGDeconstruct algebra rep a (f r)
ggdeconstructA gtoColumns = case algebraSing @algebra of
  SProduct -> \build ->
    gindex @Id @(Eval (rep Expr)) @(f r) build
      . gdeconstruct
        @(TTable Expr)
        @TColumns
        @Id
        @Expr
        @(Eval (rep Expr))
        (const fromColumns)
      . gtoColumns
  SSum ->
    gctabulate @Id @(Eval (rep Expr)) @(f r) @(a -> f r) $ \constructors as ->
      let
        (HIdentity tag, cases) =
          gdeconstructADT
            @(TTable Expr)
            @TColumns
            @Id
            @Expr
            @(Eval (rep Expr))
            (const fromColumns)
            ( \Spec{nullity} -> case nullity of
                Null -> id
                NotNull -> unsafeUnnullify
            )
            constructors
            $ gtoColumns as
        fcases = traverse1 sequence1 cases
       in
        fcases
          <&> \((_, r) :| (map (first ((tag ==.) . litExpr)) -> cases')) ->
            case_ cases' r


type GGName :: K.Algebra -> (K.Context -> Exp (Type -> Type)) -> Type -> Type
type family GGName algebra rep a where
  GGName 'K.Product rep a = GConstruct Id (Eval (rep Name)) a
  GGName 'K.Sum rep a = Name Tag -> GBuildADT Id (Eval (rep Name)) a


ggname ::
  forall algebra rep a.
  GGConstructable algebra rep =>
  (Eval (GGColumns algebra TColumns (Eval (rep Expr))) Name -> a) ->
  GGName algebra rep a
ggname gfromColumns = case algebraSing @algebra of
  SProduct ->
    gtabulate @Id @(Eval (rep Name)) @a $
      gfromColumns
        . gconstruct
          @(TTable Name)
          @TColumns
          @Id
          @Name
          @(Eval (rep Name))
          (const toColumns)
  SSum -> \tag ->
    gftabulate @Id @(Eval (rep Name)) @a $
      gfromColumns
        . gbuildADT
          @(TTable Name)
          @TColumns
          @Id
          @Name
          @(Eval (rep Name))
          (const toColumns)
          (\_ _ (Name a) -> Name a)
          (HIdentity tag)
