{-# language BlockArguments #-}
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

module Rel8.HTable ( HTable(..), hmap, hzipWith, htabulateMeta, htraverseMeta ) where

-- base
import Data.Functor.Compose ( Compose( Compose, getCompose ) )
import Data.Kind ( Type )

-- rel8
import Rel8.Context ( Context( Column ), Meta( Meta ) )
import Rel8.Info ( Column( InfoColumn ), Info )


class HTable (t :: (Meta -> Type) -> Type) where
  type HField t = (field :: Meta -> Type) | field -> t

  hfield :: t f -> HField t x -> f x
  htabulate :: forall f. (forall x. HField t x -> f x) -> t f
  htraverse :: forall f g m. Applicative m => (forall x. f x -> m (g x)) -> t f -> m (t g)
  hdbtype :: t (Column Info)


hmap :: HTable t => (forall x. f ('Meta x) -> g ('Meta x)) -> t f -> t g
hmap f t = htabulateMeta $ f <$> hfield t


hzipWith :: HTable t => (forall x. f ('Meta x) -> g ('Meta x) -> h ('Meta x)) -> t f -> t g -> t h
hzipWith f t u = htabulateMeta $ f <$> hfield t <*> hfield u


htabulateMeta :: HTable t => (forall x. HField t ('Meta x) -> f ('Meta x)) -> t f
htabulateMeta f = htabulate \i ->
  case hfield hdbtype i of
    InfoColumn _ -> f i


htraverseMeta :: (HTable t, Applicative m) => (forall x. f ('Meta x) -> m (g ('Meta x))) -> t f -> m (t g)
htraverseMeta f x = htraverse getCompose $ htabulate \i ->
  case hfield hdbtype i of
    InfoColumn _ -> Compose $ f $ hfield x i
