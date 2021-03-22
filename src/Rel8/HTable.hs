{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.HTable
  ( HTable(..)
  , hmap
  , hzipWith
  , htabulateMeta
  , htraverseMeta
  , Dict
  , Column( DictColumn )
  , ColType(..)
  ) where

-- base
import Data.Functor.Compose ( Compose( Compose, getCompose ) )
import Data.Kind ( Constraint, Type )

-- rel8
import Rel8.Context ( Context( Column ), Meta( Meta ) )
import Rel8.Info ( Column( InfoColumn ), HasInfo, Info, info )

-- semigroupoids
import Data.Functor.Apply ( Apply )


class HTable (t :: (Meta -> Type) -> Type) where
  type HField t = (field :: Meta -> Type) | field -> t
  type HAllColumns t (c :: Meta -> Constraint) :: Constraint

  hfield :: t f -> HField t x -> f x
  htabulate :: forall f. (forall x. HField t x -> f x) -> t f
  htraverse :: forall f g m. Apply m => (forall x. f x -> m (g x)) -> t f -> m (t g)
  hdict :: HAllColumns t c => t (Column (Dict c))

  hdbtype :: t (Column Info)
  default hdbtype :: HAllColumns t (ColType HasInfo) => t (Column Info)
  hdbtype = htabulate f where
    f :: forall x. HField t x -> Column Info x
    f i =
      case hfield (hdict @_ @(ColType HasInfo)) i of
        DictColumn ->
          refine @HasInfo i $ InfoColumn info


hmap :: HTable t => (forall y d x. x ~ 'Meta d y => f x -> g x) -> t f -> t g
hmap f t = htabulateMeta $ f <$> hfield t


hzipWith :: HTable t => (forall x d y. x ~ 'Meta d y => f x -> g x -> h x) -> t f -> t g -> t h
hzipWith f t u = htabulateMeta $ f <$> hfield t <*> hfield u


htabulateMeta :: HTable t => (forall x d y. x ~ 'Meta d y => HField t x -> f x) -> t f
htabulateMeta f = htabulate \i ->
  case hfield hdbtype i of
    InfoColumn _ -> f i


htraverseMeta :: (HTable t, Apply m) => (forall x d y. x ~ 'Meta d y => f x -> m (g x)) -> t f -> m (t g)
htraverseMeta f x = htraverse getCompose $ htabulate \i ->
  case hfield hdbtype i of
    InfoColumn _ -> Compose $ f $ hfield x i


data Dict :: (Meta -> Constraint) -> Type -> Type where


instance Context (Dict c) where
  data Column (Dict c) :: Meta -> Type where
    DictColumn :: c ('Meta d a) => Column (Dict c) ('Meta d a)


class ColType (c :: Type -> Constraint) (meta :: Meta) where
  refine :: p meta -> (forall d a. (meta ~ 'Meta d a, c a) => r) -> r


instance (meta ~ 'Meta d a, c a) => ColType (c :: Type -> Constraint) (meta :: Meta) where
  refine _ k = k
