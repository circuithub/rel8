{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-} 
{-# language FlexibleContexts #-} 
{-# language FlexibleInstances #-} 
{-# language FunctionalDependencies #-} 
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language QuantifiedConstraints #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

{-# options -Wno-orphans #-}

module Rel8.Schema.HKD
  ( Lift
  , HKDT(..)
  ) where

-- base
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..), runIdentity )
import Data.Kind ( Constraint, Type )
import GHC.Generics
  ( (:*:)( (:*:) ), K1( K1 ), M1( M1 ), C, D, S, Meta( MetaSel )
  , Rep
  )
import GHC.TypeLits ( KnownSymbol )
import Prelude

-- higgledy
import Data.Generic.HKD ( Construct, HKD(..), GHKD_, construct, deconstruct )

-- rel8
import Rel8.Aggregate ( Col(..), Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( Reifiable(..), SContext(..) )
import Rel8.Schema.Context.Label
  ( Labelable
  , HLabelable, hlabeler, hunlabeler
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import Rel8.Schema.Insert ( Insert, Col(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..) )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( Reify, hreify, hunreify, NotReify, notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
import Rel8.Type ( DBType )


type Column1Helper :: K.Context -> (Type -> Type) -> Type -> Constraint
class
  ( Table context (f a)
  , Context1 f ~ context
  , Context (f a) ~ context
  , Columns (f a) ~ HType a
  )
  => Column1Helper context f a | f -> context
instance
  ( Table context (f a)
  , Context1 f ~ context
  , Context (f a) ~ context
  , Columns (f a) ~ HType a
  )
  => Column1Helper context f a


type Column1 :: K.Context -> (Type -> Type) -> Constraint
class
  ( forall a. Sql DBType a => Column1Helper context f a
  , Context1 f ~ context
  ) =>
  Column1 context f | f -> context
instance
  ( forall a. Sql DBType a => Column1Helper context f a
  , Context1 f ~ context
  ) =>
  Column1 context f


type Context1 :: (Type -> Type) -> K.Context
type Context1 f = Context (f Bool)


toColumn1 :: forall a f context. (Column1 context f, Sql DBType a)
  => f a -> HType a (Col context)
toColumn1 = case Dict @(Column1Helper context f) @a of
  Dict -> toColumns


fromColumn1 :: forall a f context. (Column1 context f, Sql DBType a)
  => HType a (Col context) -> f a
fromColumn1 = case Dict @(Column1Helper context f) @a of
  Dict -> fromColumns


type Recontextualize1
  :: K.Context
  -> K.Context
  -> (Type -> Type)
  -> (Type -> Type)
  -> Constraint
class Recontextualize context context' (f Bool) (f' Bool) =>
  Recontextualize1 context context' f f'
instance Recontextualize context context' (f Bool) (f' Bool) =>
  Recontextualize1 context context' f f'


class HTable (GColumns rep) => GTable rep where
  toGColumns :: HLabelable context
    => (forall a. Sql DBType a => f a -> HType a context)
    -> GHKD_ f rep x
    -> GColumns rep context
  fromGColumns :: HLabelable context
    => (forall a. Sql DBType a => HType a context -> f a)
    -> GColumns rep context
    -> GHKD_ f rep x


instance GTable rep => GTable (M1 D c rep) where
  toGColumns f (M1 a) = toGColumns f a
  {-# INLINABLE toGColumns #-}

  fromGColumns f = M1 . fromGColumns f
  {-# INLINABLE fromGColumns #-}


instance GTable rep => GTable (M1 C c rep) where
  toGColumns f (M1 a) = toGColumns f a
  {-# INLINABLE toGColumns #-}

  fromGColumns f = M1 . fromGColumns f
  {-# INLINABLE fromGColumns #-}


instance (KnownSymbol name, Sql DBType a) =>
  GTable (M1 S ('MetaSel ('Just name) _su _ss _ds) (K1 i a))
 where
  toGColumns f (M1 (K1 a)) = hlabel hlabeler (f a)
  {-# INLINABLE toGColumns #-}

  fromGColumns f a = M1 (K1 (f (hunlabel hunlabeler a)))
  {-# INLINABLE fromGColumns #-}


instance (GTable f, GTable g) => GTable (f :*: g) where
  toGColumns f (x :*: y) = HProduct (toGColumns f x) (toGColumns f y)
  {-# INLINABLE toGColumns #-}

  fromGColumns f (HProduct x y) = fromGColumns f x :*: fromGColumns f y
  {-# INLINABLE fromGColumns #-}


type GRep a = GColumns (Rep a)


type GColumns :: (Type -> Type) -> K.HTable
type family GColumns rep where
  GColumns (M1 D _ f) = GColumns f
  GColumns (M1 C _ f) = GColumns f
  GColumns (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ a)) =
    HLabel name (HType a)
  GColumns (f :*: g) = HProduct (GColumns f) (GColumns g)


instance
  ( GTable (Rep a)
  , Column1 context f
  , Labelable context
  , NotReify context
  )
  => Table context (HKD a f)
 where
  type Columns (HKD a f) = GRep a
  type Context (HKD a f) = Context1 f

  toColumns = toGColumns toColumn1 . runHKD
  fromColumns = HKD . fromGColumns fromColumn1
  reify = notReify
  unreify = notReify


instance
  ( a ~ a'
  , GTable (Rep a)
  , NotReify context, NotReify context'
  , Recontextualize1 context context' f f'
  , Column1 context f, Labelable context
  , Column1 context' f', Labelable context'
  ) =>
  Recontextualize
    context
    context'
    (HKD a f)
    (HKD a' f')


type Lift :: K.Context -> Type -> Type
type family Lift context a where
  Lift (Reify context) a = ALift context a
  Lift Aggregate a = HKD a (Compose Aggregate Expr)
  Lift Expr a = HKD a Expr
  Lift Insert a = HKD a Expr
  Lift Name a = HKD a Name
  Lift Result a = a


type ALift :: K.Context -> Type -> Type
newtype ALift context a = ALift
  { unALift :: Lift context a
  }


instance
  ( Reifiable context
  , GTable (Rep a)
  , Construct Identity a
  )
  => Table (Reify context) (ALift context a)
 where
  type Context (ALift context a) = Reify context
  type Columns (ALift context a) = GRep a
  type Unreify (ALift context a) = Lift context a

  fromColumns = sfromColumnsLift contextSing
  toColumns = stoColumnsLift contextSing
  reify _ = ALift
  unreify _ (ALift a) = a


instance
  ( Reifiable context
  , Reifiable context'
  , GTable (Rep a)
  , Construct Identity a
  )
  => Recontextualize
    (Reify context)
    (Reify context')
    (ALift context a)
    (ALift context' a)


sfromColumnsLift :: forall a context. (GTable (Rep a), Construct Identity a)
  => SContext context
  -> GRep a (Col (Reify context))
  -> ALift context a
sfromColumnsLift = \case
  SAggregate ->
    ALift .
    HKD .
    fromGColumns (\(HType (Aggregation a)) -> Compose a) .
    hunreify
  SExpr -> ALift . fromColumns . hunreify
  SInsert ->
    ALift .
    HKD .
    fromGColumns (\(HType (RequiredInsert a)) -> a) .
    hunreify
  SName -> ALift . fromColumns . hunreify
  SResult -> ALift . runIdentity . construct . fromColumns . hunreify
  SReify context -> ALift . sfromColumnsLift context . hunreify
  

stoColumnsLift :: forall a context. (GTable (Rep a), Construct Identity a)
  => SContext context
  -> ALift context a
  -> GRep a (Col (Reify context))
stoColumnsLift = \case
  SAggregate ->
    hreify .
    toGColumns (\(Compose a) -> HType (Aggregation a)) .
    runHKD .
    unALift
  SExpr -> hreify . toColumns . unALift
  SInsert ->
    hreify .
    toGColumns (HType . RequiredInsert) .
    runHKD .
    unALift
  SName -> hreify . toColumns . unALift
  SResult -> hreify . toColumns . deconstruct @Identity . unALift
  SReify context -> hreify . stoColumnsLift context . unALift


type HKDT :: Type -> Type
newtype HKDT a = HKDT
  { unHKDT :: a
  }


instance (GTable (Rep a), Construct Identity a, x ~ HKD a Expr) =>
  ToExprs x (HKDT a)
 where
  toResult = toColumns . deconstruct @Identity . unHKDT
  fromResult = HKDT . runIdentity . construct . fromColumns


type instance FromExprs (HKD a Expr) = a
