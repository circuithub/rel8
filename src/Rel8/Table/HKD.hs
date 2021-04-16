{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.HKD
  ( HKD( HKD )
  , HKDable, fromHKD, toHKD, HKDT(..)
  , BuildableHKD
  , BuildHKD, buildHKD
  , InsertHKD, insertHKD
  , ConstructableHKD
  , ConstructHKD, constructHKD
  , DeconstructHKD, deconstructHKD
  , NameHKD, nameHKD
  , AggregateHKD, aggregateHKD
  , HKDRep
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import Data.Void ( Void )
import GHC.Generics ( Generic, Rep, from, to )
import GHC.TypeLits ( Symbol )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Column ( TColumn )
import Rel8.Expr ( Expr )
import Rel8.FCF ( Eval, Exp )
import Rel8.Kind.Algebra ( KnownAlgebra )
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
import Rel8.Generic.Map ( GMap, GMappable, gmap, gunmap )
import Rel8.Generic.Record ( GRecord, GRecordable, grecord, gunrecord )
import Rel8.Generic.Rel8able
  ( Rel8able
  , GColumns, gfromColumns, gtoColumns
  , greify, gunreify
  , TUnreifyContext
  )
import Rel8.Generic.Table
  ( GGTable, GGColumns, GGContext, ggfromColumns, ggtoColumns
  , GAlgebra
  )
import Rel8.Schema.Context ( Col )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.Insert ( Insert )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Col( Reify ), Reify, hreify, hunreify, notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Serialize ( Encodable, Encoding, Constraints )
import Rel8.Table
  ( Table, Columns, Context, Unreify
  , fromColumns, toColumns, reify, unreify
  , TTable, TColumns, TUnreify
  )
import Rel8.Type.Serialize ( Strategy, ExprsFor, encode, decode )


type GColumnsHKD :: Type -> K.HTable
type GColumnsHKD a =
  Eval (GGColumns (GAlgebra (Rep a)) TColumns (GRecord (GMap (TColumn (Reify Result)) (Rep a))))


type HKD :: Type -> K.Rel8able
newtype HKD a f = HKD (GColumnsHKD a (Col f))


instance HKDable a => Rel8able (HKD a) where
  type GColumns (HKD a) = GColumnsHKD a

  gfromColumns = HKD
  gtoColumns (HKD a) = a

  greify (HKD a) = HKD (hreify a)
  gunreify (HKD a) = HKD (hunreify a)


instance
  ( KnownAlgebra (GAlgebra (Rep a))
  , HTable (GColumnsHKD a)
  , Eval (GGTable (GAlgebra (Rep a)) (TTable (Reify f)) TColumns (Col (Reify f)) (GRecord (GMap (TColumn (Reify f)) (Rep a))))
  , Eval (GGColumns (GAlgebra (Rep a)) TColumns (GRecord (GMap (TColumn (Reify f)) (Rep a)))) ~ GColumnsHKD a
  , Eval (GGContext (GAlgebra (Rep a)) TUnreifyContext (GRecord (GMap (TColumn (Reify f)) (Rep a)))) ~ f
  , GRecordable (GMap (TColumn (Reify f)) (Rep a))
  , GMappable (TTable (Reify f)) (GMap (TColumn (Reify f)) (Rep a))
  , GMap TUnreify (GMap (TColumn (Reify f)) (Rep a)) ~ GMap (TColumn f) (Rep a)
  )
  => Generic (HKD a f)
 where
  type Rep (HKD a f) = GMap (TColumn f) (Rep a)

  from =
    gmap @(TTable (Reify f)) (Proxy @TUnreify) (unreify Refl) .
    gunrecord @(GMap (TColumn (Reify f)) (Rep a)) .
    ggfromColumns
      @(GAlgebra (Rep a))
      @(TTable (Reify f))
      @TColumns
      (\(Reify a) -> a)
      Reify
      fromColumns .
    hreify .
    (\(HKD a) -> a)

  to =
    HKD .
    hunreify .
    ggtoColumns
      @(GAlgebra (Rep a))
      @(TTable (Reify f))
      @TColumns
      (\(Reify a) -> a)
      Reify
      toColumns .
    grecord @(GMap (TColumn (Reify f)) (Rep a)) .
    gunmap @(TTable (Reify f)) (Proxy @TUnreify) (reify Refl)


type HKDT :: Type -> Type
newtype HKDT a = HKDT
  { unHKDT :: a
  }
  deriving newtype Generic


instance HKDable a => Table Result (HKDT a) where
  type Columns (HKDT a) = GColumnsHKD a
  type Context (HKDT a) = Result
  type Unreify (HKDT a) = Void

  fromColumns = HKDT . fromHKD . HKD
  toColumns = (\(HKD a) -> a) . toHKD . (\(HKDT a) -> a)
  reify = notReify
  unreify = notReify


instance HKDable (HKDT a) => Encodable (HKDT a) where
  type Encoding (HKDT a) = Proxy HKD


instance Strategy (Proxy HKD) where
  type ExprsFor (Proxy HKD) a = HKD a Expr

  encode = (\(HKD a) -> a) . toHKD
  decode = fromHKD . HKD


type instance Constraints (Proxy HKD) a = HKDable a


fromHKD :: HKDable a => HKD a Result -> a
fromHKD = to . gunmap @Top (Proxy @(TColumn Result)) id . from


toHKD :: HKDable a => a -> HKD a Result
toHKD = to . gmap @Top (Proxy @(TColumn Result)) id . from


class Top_
instance Top_


data Top :: Type -> Exp Constraint
type instance Eval (Top _) = Top_


class
  ( Generic a
  , HTable (GColumns (HKD a))
  , KnownAlgebra (GAlgebra (Rep a))
  , Eval (GGTable (GAlgebra (Rep a)) (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (GMap (TColumn (Reify Result)) (Rep a))))
  , Eval (GGContext (GAlgebra (Rep a)) TUnreifyContext (GRecord (GMap (TColumn (Reify Result)) (Rep a)))) ~ Result
  , GRecordable (GMap (TColumn (Reify Result)) (Rep a))
  , GMappable Top (Rep a)
  , GMappable (TTable (Reify Result)) (GMap (TColumn (Reify Result)) (Rep a))
  , GMap TUnreify (GMap (TColumn (Reify Result)) (Rep a)) ~ GMap (TColumn Result) (Rep a)
  , Table Expr (HKD a Expr)
  , Columns (HKD a Expr) ~ GColumns (HKD a)
  )
  => HKDable a
instance
  ( Generic a
  , HTable (GColumns (HKD a))
  , KnownAlgebra (GAlgebra (Rep a))
  , Eval (GGTable (GAlgebra (Rep a)) (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (GMap (TColumn (Reify Result)) (Rep a))))
  , Eval (GGContext (GAlgebra (Rep a)) TUnreifyContext (GRecord (GMap (TColumn (Reify Result)) (Rep a)))) ~ Result
  , GRecordable (GMap (TColumn (Reify Result)) (Rep a))
  , GMappable Top (Rep a)
  , GMappable (TTable (Reify Result)) (GMap (TColumn (Reify Result)) (Rep a))
  , GMap TUnreify (GMap (TColumn (Reify Result)) (Rep a)) ~ GMap (TColumn Result) (Rep a)
  , Table Expr (HKD a Expr)
  , Columns (HKD a Expr) ~ GColumns (HKD a)
  )
  => HKDable a


type BuildableHKD :: Type -> Symbol -> Constraint
class GGBuildable (GAlgebra (Rep a)) name (HKDRep a) => BuildableHKD a name
instance GGBuildable (GAlgebra (Rep a)) name (HKDRep a) => BuildableHKD a name


type BuildHKD :: Type -> Symbol -> Type
type BuildHKD a name = GGBuild (GAlgebra (Rep a)) name (HKDRep a) (HKD a Expr)


buildHKD :: forall a name. BuildableHKD a name => BuildHKD a name
buildHKD =
  ggbuild @(GAlgebra (Rep a)) @name @(HKDRep a) @(HKD a Expr) HKD


type InsertHKD :: Type -> Symbol -> Type
type InsertHKD a name = GGInsert (GAlgebra (Rep a)) name (HKDRep a) (HKD a Insert)


insertHKD :: forall a name. BuildableHKD a name => InsertHKD a name
insertHKD =
  gginsert @(GAlgebra (Rep a)) @name @(HKDRep a) @(HKD a Insert) HKD


type ConstructableHKD :: Type -> Constraint
class GGConstructable (GAlgebra (Rep a)) (HKDRep a) => ConstructableHKD a
instance GGConstructable (GAlgebra (Rep a)) (HKDRep a) => ConstructableHKD a


type ConstructHKD :: Type -> Type
type ConstructHKD a = forall r. GGConstruct (GAlgebra (Rep a)) (HKDRep a) r


constructHKD :: forall a. ConstructableHKD a => ConstructHKD a -> HKD a Expr
constructHKD f =
  ggconstruct @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Expr) HKD
    (f @(HKD a Expr))


type DeconstructHKD :: Type -> Type -> Type
type DeconstructHKD a r = GGDeconstruct (GAlgebra (Rep a)) (HKDRep a) (HKD a Expr) r


deconstructHKD :: forall a r. (ConstructableHKD a, Table Expr r)
  => DeconstructHKD a r
deconstructHKD = ggdeconstruct @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Expr) @r (\(HKD a) -> a)


type NameHKD :: Type -> Type
type NameHKD a = GGName (GAlgebra (Rep a)) (HKDRep a) (HKD a Name)


nameHKD :: forall a. ConstructableHKD a => NameHKD a
nameHKD = ggname @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Name) HKD


type AggregateHKD :: Type -> Type
type AggregateHKD a = forall r. GGAggregate (GAlgebra (Rep a)) (HKDRep a) r


aggregateHKD :: forall a. ConstructableHKD a
  => AggregateHKD a -> HKD a Expr -> Aggregate (HKD a Expr)
aggregateHKD f =
  ggaggregate @(GAlgebra (Rep a)) @(HKDRep a) @(HKD a Expr) HKD (\(HKD a) -> a)
    (f @(Aggregate (HKD a Expr)))


data HKDRep :: Type -> K.Context -> Exp (Type -> Type)
type instance Eval (HKDRep a context) =
  GRecord (GMap (TColumn context) (Rep a))
