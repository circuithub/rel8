{-# language BlockArguments #-} 
{-# language DataKinds #-} 
{-# language FlexibleContexts #-} 
{-# language FlexibleInstances #-} 
{-# language FunctionalDependencies #-} 
{-# language GADTs #-} 
{-# language LambdaCase #-} 
{-# language MultiParamTypeClasses #-} 
{-# language NamedFieldPuns #-} 
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

{-# options -Wno-orphans #-}

module Rel8.Schema.HKD ( Lift, FlipHKD(..) ) where

-- base
import Control.Applicative ( Const(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..), runIdentity )
import Data.Kind ( Constraint, Type )
import GHC.Generics
import GHC.TypeLits ( KnownSymbol )
import Prelude

-- higgledy
import Data.Generic.HKD ( Construct, HKD( HKD, runHKD ), GHKD_, construct, deconstruct )

-- rel8
import Rel8.Aggregate ( Col(..), Aggregate )
import Rel8.Expr ( Col(..), Expr )
import Rel8.Kind.Necessity
import Rel8.Schema.Context
import Rel8.Schema.Field ( Reify, Reifiable(..), SContext(..), hunreify, hreify )
import Rel8.Schema.HTable
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Pair ( HPair( HPair ) )
import Rel8.Schema.Insert ( Insert, Col(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Col(..), Name(..) )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


type HKDT :: K.Context -> Constraint
class HKDT context where
  type FieldContext context :: Type -> Type
  toHKDT :: Col context ('Spec name 'Required a) -> FieldContext context a
  fromHKDT :: FieldContext context a -> Col context ('Spec name 'Required a)


instance HKDT Expr where
  type FieldContext Expr = Expr
  toHKDT (DB expr) = expr
  fromHKDT = DB


instance HKDT Name where
  type FieldContext Name = Const String
  toHKDT (NameCol s) = Const s
  fromHKDT (Const s) = NameCol s


instance HKDT Identity where
  type FieldContext Identity = Identity
  toHKDT (Result x) = pure x
  fromHKDT (Identity x) = Result x


instance HKDT Aggregate where
  type FieldContext Aggregate = Compose Aggregate Expr
  toHKDT (Aggregation x) = Compose x
  fromHKDT (Compose x) = Aggregation x


instance HKDT Insert where
  type FieldContext Insert = Expr
  toHKDT (RequiredInsert x) = x
  fromHKDT = RequiredInsert


newtype FlipHKD f a = FlipHKD { unFlipHKD :: HKD a (FieldContext f) }


instance (HKDSpec (Rep a), HKDT f, f ~ g) => Table f (FlipHKD g a) where
  type Columns (FlipHKD g a) = GHTable a
  type Context (FlipHKD g a) = g
  toColumns = fromGHKD . runHKD . unFlipHKD
  fromColumns = FlipHKD . HKD . toGHKD


class HTable (GHTable_ rep) => HKDSpec rep where
  fromGHKD :: HKDT f => GHKD_ (FieldContext f) rep x -> GHTable_ rep (Col f)
  toGHKD :: HKDT f => GHTable_ rep (Col f) -> GHKD_ (FieldContext f) rep x


instance (HKDSpec f, HTable (GHTable_ (M1 D c f))) => HKDSpec (M1 D c f) where
  fromGHKD (M1 x) = fromGHKD x
  toGHKD = M1 . toGHKD


instance (HKDSpec f, HTable (GHTable_ (M1 C c f))) => HKDSpec (M1 C c f) where
  fromGHKD (M1 x) = fromGHKD x
  toGHKD = M1 . toGHKD


instance (KnownSymbol name, Sql DBType a) => HKDSpec (M1 S ('MetaSel ('Just name) x y z) (K1 i a)) where
  fromGHKD (M1 (K1 a)) = HIdentity $ fromHKDT a
  toGHKD (HIdentity col) = M1 $ K1 $ toHKDT col


instance (HKDSpec f, HKDSpec g) => HKDSpec (f :*: g) where
  fromGHKD (x :*: y) = HPair (fromGHKD x) (fromGHKD y)
  toGHKD (HPair x y) = toGHKD x :*: toGHKD y
  

type GHTable a = GHTable_ (Rep a)


type GHTable_ :: (Type -> Type) -> K.HTable
type family GHTable_ rep where
  GHTable_ (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ x)) = HIdentity ('Spec '[ name ] 'Required x)
  GHTable_ (M1 D _ f) = GHTable_ f
  GHTable_ (M1 C _ f) = GHTable_ f
  GHTable_ (f :*: g) = HPair (GHTable_ f) (GHTable_ g)


type Lift :: K.Context -> Type -> Type
type family Lift context a where
  Lift (Reify context) a = ALift context a
  Lift Identity a = Identity a
  Lift f a = FlipHKD f a


type ALift :: K.Context -> Type -> Type
newtype ALift context a = ALift { runALift :: Lift context a }


instance 
  ( Reifiable context
  , HTable (GHTable a)
  , HKDSpec (Rep a)
  , Generic a
  , Construct Identity a
  ) => Table (Reify context) (ALift context a) where
  type Context (ALift context a) = Reify context
  type Columns (ALift context a) = GHTable a

  fromColumns = sfromColumnsLift contextSing
  toColumns = stoColumnsLift contextSing


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  , GHTable a ~ GHTable a'
  , HKDSpec (Rep a), HKDSpec (Rep a')
  , Generic a, Generic a'
  , Construct Identity a, Construct Identity a'
  ) =>
    Recontextualize
    (Reify context)
    (Reify context')
    (ALift context a)
    (ALift context' a')


sfromColumnsLift :: forall a context. (HKDSpec (Rep a), Generic a, Construct Identity a)
  => SContext context
  -> GHTable a (Col (Reify context))
  -> ALift context a
sfromColumnsLift = \case
  SExpr -> ALift . fromColumns . hunreify
  SName -> ALift . fromColumns . hunreify
  SAggregate -> ALift . fromColumns . hunreify
  SIdentity -> ALift . construct . HKD @a . toGHKD @(Rep a) . hunreify
  SReify context -> ALift . sfromColumnsLift context . hunreify
  SInsert -> ALift . fromColumns . hunreify
  

stoColumnsLift :: forall a context. (HKDSpec (Rep a), Generic a, Construct Identity a)
  => SContext context
  -> ALift context a
  -> GHTable a (Col (Reify context))
stoColumnsLift = \case
  SExpr -> hreify . toColumns . runALift
  SName -> hreify . toColumns . runALift
  SAggregate -> hreify . toColumns . runALift
  SIdentity -> hreify . toColumns . FlipHKD . deconstruct . runIdentity . runALift
  SReify context -> hreify . stoColumnsLift context . runALift
  SInsert -> hreify . toColumns . runALift
