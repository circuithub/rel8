{-# language AllowAmbiguousTypes #-} 
{-# language BlockArguments #-} 
{-# language DataKinds #-} 
{-# language FlexibleContexts #-} 
{-# language FlexibleInstances #-} 
{-# language GADTs #-} 
{-# language LambdaCase #-} 
{-# language MultiParamTypeClasses #-} 
{-# language NamedFieldPuns #-} 
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

{-# options -Wno-orphans #-}

module Rel8.Schema.HKD ( Lift, FlipHKD(..) ) where

-- base
import Data.Functor.Compose ( Compose )
import Data.Functor.Identity ( Identity, runIdentity )
import Data.Kind ( Type )
import GHC.Generics
import Prelude

-- higgledy
import Data.Generic.HKD ( Construct, HKD, construct, deconstruct )

-- rel8
import Rel8.Aggregate ( Col(..), Aggregate )
import Rel8.Expr ( Col(..), Expr )
import Rel8.Kind.Necessity
import Rel8.Schema.Context
import Rel8.Schema.Dict
import Rel8.Schema.Field ( Reify, Reifiable(..), SContext(..), hunreify, hreify )
import Rel8.Schema.Generic
import Rel8.Schema.HTable
import Rel8.Schema.HTable.Identity ( HIdentity )
import Rel8.Schema.HTable.Pair ( HPair )
import Rel8.Schema.Insert ( Insert, Col(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Table
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize


newtype FlipHKD f a = FlipHKD { unFlipHKD :: HKD a f }


type GHTable a = GHTable_ (Rep a)


type GHTable_ :: (Type -> Type) -> K.HTable
type family GHTable_ rep where
  GHTable_ (M1 S ('MetaSel ('Just name) _ _ _) (K1 _ x)) = HIdentity ('Spec '[ name ] 'Required x)
  GHTable_ (M1 _ _ f) = GHTable_ f
  GHTable_ (f :*: g) = HPair (GHTable_ f) (GHTable_ g)


type Lift :: K.Context -> Type -> Type
type family Lift context a where
  Lift (Reify context) a = ALift context a
  Lift Identity a = Identity a
  Lift f a = FlipHKD f a


newtype InsertExpr a = InsertExpr { toExpr :: Expr a }


type ALift :: K.Context -> Type -> Type
newtype ALift context a = ALift { runALift :: Lift context a }


instance (Reifiable context, HTable (GHTable a), Generic a, Construct Identity a, HConstrainTable (GHTable a) HKDFieldSpec)
  => Table (Reify context) (ALift context a)
 where
  type Context (ALift context a) = Reify context
  type Columns (ALift context a) = GHTable a

  fromColumns = sfromColumnsLift contextSing
  toColumns = stoColumnsLift contextSing


instance
  ( Reifiable context, Reifiable context'
  , Recontextualize (Reify context) (Reify context') a a'
  , HTable (GHTable a)
  , GHTable a ~ GHTable a'
  , Generic a, Generic a'
  , Construct Identity a, Construct Identity a'
  , HConstrainTable (GHTable a) HKDFieldSpec
  ) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (ALift context a)
    (ALift context' a')


class HKDFieldSpec spec where
  withHKDFieldSpec 
    :: SSpec spec 
    -> (forall name x. spec ~ 'Spec name 'Required x => r) 
    -> r


instance spec ~ 'Spec name 'Required x => HKDFieldSpec spec where
  withHKDFieldSpec _ = id


sfromColumnsLift :: forall a context. (HTable (GHTable a), Generic a, Construct Identity a, HConstrainTable (GHTable a) HKDFieldSpec)
  => SContext context
  -> GHTable a (Col (Reify context))
  -> ALift context a
sfromColumnsLift = \case
  SExpr -> ALift . fromColumns . hunreify
  SName -> ALift . fromColumns . hunreify
  SIdentity -> ALift . construct . unFlipHKD . fromColumns . hunreify
  SReify context -> ALift . sfromColumnsLift context . hunreify
  SInsert -> ALift . fromColumns . hunreify


stoColumnsLift :: (HTable (GHTable a), Generic a, Construct Identity a, HConstrainTable (GHTable a) HKDFieldSpec)
  => SContext context
  -> ALift context a
  -> GHTable a (Col (Reify context))
stoColumnsLift = \case
  SExpr -> hreify . toColumns . runALift
  SName -> hreify . toColumns . runALift
  SIdentity -> hreify . toColumns . FlipHKD . deconstruct . runIdentity . runALift
  SReify context -> hreify . stoColumnsLift context . runALift
  SInsert -> hreify . toColumns . runALift
  SAggregate -> hreify . toColumns . runALift


instance (f ~ g, HTable (GHTable a)) => Table f (FlipHKD g a) where
  type Columns (FlipHKD g a) = GHTable a
  type Context (FlipHKD g a) = g


instance HTable (GHTable a) => ToExprs a (FlipHKD Expr a)


type instance FromExprs (FlipHKD Expr a) = a
