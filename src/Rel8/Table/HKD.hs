{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rel8.Table.HKD (
  HKD (HKD),
  HKDable,
  BuildableHKD,
  BuildHKD,
  buildHKD,
  ConstructableHKD,
  ConstructHKD,
  constructHKD,
  DeconstructHKD,
  deconstructHKD,
  deconstructAHKD,
  NameHKD,
  nameHKD,
  HKDRep,
)
where

-- base
import Data.Kind (Constraint, Type)
import GHC.Generics (Generic, Rep, from, to)
import GHC.TypeLits (Symbol)
import Prelude

-- rel8
import Rel8.Column (TColumn)
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
import Rel8.Generic.Map (GMap)
import Rel8.Generic.Record (
  GRecord,
  GRecordable,
  Record (Record),
  grecord,
  gunrecord,
  unrecord,
 )
import Rel8.Generic.Rel8able (
  GColumns,
  GFromExprs,
  Rel8able,
  gfromColumns,
  gfromResult,
  gtoColumns,
  gtoResult,
 )
import Rel8.Generic.Table (
  GAlgebra,
  GGColumns,
  GGSerialize,
  ggfromResult,
  ggtoResult,
 )
import Rel8.Generic.Table.Record (GContext, GTable)
import qualified Rel8.Generic.Table.Record as G
import Rel8.Kind.Algebra (KnownAlgebra)
import Rel8.Schema.HTable (HTable)
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name (Name)
import Rel8.Schema.Result (Result)
import Rel8.Table (
  TColumns,
  TContext,
  TSerialize,
  TTable,
  Table,
  fromColumns,
  fromResult,
  toColumns,
  toResult,
 )

-- semigroupoids
import Data.Functor.Apply (Apply)


type GColumnsHKD :: Type -> K.HTable
type GColumnsHKD a =
  Eval (GGColumns (GAlgebra (Rep a)) TColumns (GRecord (GMap (TColumn Expr) (Rep a))))


type HKD :: Type -> K.Rel8able
newtype HKD a f = HKD (GColumnsHKD a f)


instance HKDable a => Rel8able (HKD a) where
  type GColumns (HKD a) = GColumnsHKD a
  type GFromExprs (HKD a) = a


  gfromColumns _ = HKD
  gtoColumns _ (HKD a) = a


  gfromResult =
    unrecord
      . to
      . ggfromResult
        @(GAlgebra (Rep a))
        @TSerialize
        @TColumns
        @(Eval (HKDRep a Expr))
        @(Eval (HKDRep a Result))
        (\(_ :: proxy x) -> fromResult @_ @x)


  gtoResult =
    ggtoResult
      @(GAlgebra (Rep a))
      @TSerialize
      @TColumns
      @(Eval (HKDRep a Expr))
      @(Eval (HKDRep a Result))
      (\(_ :: proxy x) -> toResult @_ @x)
      . from
      . Record


instance
  ( GTable (TTable f) TColumns (GRecord (GMap (TColumn f) (Rep a)))
  , G.GColumns TColumns (GRecord (GMap (TColumn f) (Rep a))) ~ GColumnsHKD a
  , GContext TContext (GRecord (GMap (TColumn f) (Rep a))) ~ f
  , GRecordable (GMap (TColumn f) (Rep a))
  ) =>
  Generic (HKD a f)
  where
  type Rep (HKD a f) = GMap (TColumn f) (Rep a)


  from =
    gunrecord @(GMap (TColumn f) (Rep a))
      . G.gfromColumns
        @(TTable f)
        @TColumns
        fromColumns
      . (\(HKD a) -> a)


  to =
    HKD
      . G.gtoColumns
        @(TTable f)
        @TColumns
        toColumns
      . grecord @(GMap (TColumn f) (Rep a))


type HKDable :: Type -> Constraint
class
  ( Generic (Record a)
  , HTable (GColumns (HKD a))
  , KnownAlgebra (GAlgebra (Rep a))
  , Eval (GGSerialize (GAlgebra (Rep a)) TSerialize TColumns (Eval (HKDRep a Expr)) (Eval (HKDRep a Result)))
  , GRecord (GMap (TColumn Result) (Rep a)) ~ Rep (Record a)
  ) =>
  HKDable a
instance
  ( Generic (Record a)
  , HTable (GColumns (HKD a))
  , KnownAlgebra (GAlgebra (Rep a))
  , Eval (GGSerialize (GAlgebra (Rep a)) TSerialize TColumns (Eval (HKDRep a Expr)) (Eval (HKDRep a Result)))
  , GRecord (GMap (TColumn Result) (Rep a)) ~ Rep (Record a)
  ) =>
  HKDable a


type Top_ :: Constraint
class Top_
instance Top_


data Top :: Type -> Exp Constraint
type instance Eval (Top _) = Top_


type BuildableHKD :: Type -> Symbol -> Constraint
class GGBuildable (GAlgebra (Rep a)) name (HKDRep a) => BuildableHKD a name
instance GGBuildable (GAlgebra (Rep a)) name (HKDRep a) => BuildableHKD a name


type BuildHKD :: Type -> Symbol -> Type
type BuildHKD a name = GGBuild (GAlgebra (Rep a)) name (HKDRep a) (HKD a Expr)


buildHKD :: forall a name. BuildableHKD a name => BuildHKD a name
buildHKD =
  ggbuild @(GAlgebra (Rep a)) @name @(HKDRep a) @(HKD a Expr) HKD


type ConstructableHKD :: Type -> Constraint
class GGConstructable (GAlgebra (Rep a)) (HKDRep a) => ConstructableHKD a
instance GGConstructable (GAlgebra (Rep a)) (HKDRep a) => ConstructableHKD a


type ConstructHKD :: Type -> Type
type ConstructHKD a = forall r. GGConstruct (GAlgebra (Rep a)) (HKDRep a) r


constructHKD :: forall a. ConstructableHKD a => ConstructHKD a -> HKD a Expr
constructHKD f =
  ggconstruct @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Expr)
    HKD
    (f @(HKD a Expr))


type DeconstructHKD :: Type -> Type -> Type
type DeconstructHKD a r = GGDeconstruct (GAlgebra (Rep a)) (HKDRep a) (HKD a Expr) r


deconstructHKD ::
  forall a r.
  (ConstructableHKD a, Table Expr r) =>
  DeconstructHKD a r
deconstructHKD = ggdeconstruct @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Expr) @r (\(HKD a) -> a)


deconstructAHKD ::
  forall a f r.
  (ConstructableHKD a, Apply f, Table Expr r) =>
  DeconstructHKD a (f r)
deconstructAHKD = ggdeconstructA @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Expr) @f @r (\(HKD a) -> a)


type NameHKD :: Type -> Type
type NameHKD a = GGName (GAlgebra (Rep a)) (HKDRep a) (HKD a Name)


nameHKD :: forall a. ConstructableHKD a => NameHKD a
nameHKD = ggname @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Name) HKD


data HKDRep :: Type -> K.Context -> Exp (Type -> Type)
type instance
  Eval (HKDRep a context) =
    GRecord (GMap (TColumn context) (Rep a))
