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

module Rel8.HTable ( HTable(..), hmap, hzipWith ) where

-- base
import Data.Kind ( Type )

-- rel8
import Rel8.Context ( Context, KContext )
import Rel8.Info ( Info )


class HTable (t :: KContext -> Type) where
  type HField t = (field :: Type -> Type) | field -> t

  hfield :: t (Context f) -> HField t x -> f x
  htabulate :: forall f. (forall x. HField t x -> f x) -> t (Context f)
  htraverse :: forall f g m. Applicative m => (forall x. f x -> m (g x)) -> t (Context f) -> m (t (Context g))
  hdbtype :: t (Context Info)


hmap :: HTable t => (forall x. f x -> g x) -> t (Context f) -> t (Context g)
hmap f t = htabulate $ f <$> hfield t


hzipWith :: HTable t => (forall x. f x -> g x -> h x) -> t (Context f) -> t (Context g) -> t (Context h)
hzipWith f t u = htabulate $ f <$> hfield t <*> hfield u
