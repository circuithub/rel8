{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}

module Rel8.HTable ( HTable(..), hmap, hzipWith, htabulateMeta, htraverseMeta, Dict, hdict, Column( DictColumn ) ) where

-- base
import Data.Functor.Compose ( Compose( Compose, getCompose ) )
import Data.Kind ( Constraint, Type )

-- rel8
import Rel8.Context ( Context( Column ), Meta( Meta ) )
import Rel8.Info ( Column( InfoColumn ), Info )

-- semigroupoids
import Data.Functor.Apply ( Apply )


class HTable (t :: (Meta -> Type) -> Type) where
  type HField t = (field :: Meta -> Type) | field -> t
  type HAllColumns t (c :: Type -> Constraint) :: Constraint

  hfield :: t f -> HField t x -> f x
  htabulate :: forall f. (forall x. HField t x -> f x) -> t f
  htraverse :: forall f g m. Apply m => (forall x. f x -> m (g x)) -> t f -> m (t g)
  hdbtype :: t (Column Info)
  hdict :: HAllColumns t c => t (Column (Dict c))


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


data Dict :: (Type -> Constraint) -> Type -> Type where


instance Context (Dict c) where
  data Column (Dict c) :: Meta -> Type where
    DictColumn :: c a => Column (Dict c) ('Meta d a)
