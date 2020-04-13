{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Schema where

import Control.Applicative ( Const(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Tagged.PolyKinded ( Tagged(..) )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Rel8.Table ( Table(..) )


newtype Schema a = Schema (Pattern a (Const String))


genericSchema :: InferSchema (Pattern a) => Schema a
genericSchema = Schema inferSchema


class HRepresentable f => InferSchema (f :: (Type -> Type) -> Type) where
  inferSchema :: f (Const String)


instance InferSchema f => InferSchema (Compose (Tagged (t :: *)) f) where
  inferSchema = Compose $ Tagged inferSchema


instance (InferSchema l, InferSchema r) => InferSchema (HProduct l r) where
  inferSchema = HProduct inferSchema inferSchema


instance KnownSymbol name => InferSchema (Compose (Tagged name) (HIdentity x)) where
  inferSchema = Compose $ Tagged $ HIdentity $ Const $ symbolVal $ Proxy @name
