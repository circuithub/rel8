{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Schema where

import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Data.Functor.Compose ( Compose(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Kind ( Type )
import Data.Tagged.PolyKinded ( Tagged(..) )
import Rel8.Primitive ( PrimLike(..) )
import Rel8.Table ( Table(..) )


newtype Schema a = Schema (Pattern a Schema)


unSchema :: Schema a -> Pattern a Schema
unSchema (Schema x) = x


instance PrimLike Schema where
  data Primitive a Schema = PrimSchema String
    deriving (Show)


genericSchema :: InferSchema (Pattern a) => Schema a
genericSchema = Schema inferSchema


class HRepresentable f => InferSchema (f :: (Type -> Type) -> Type) where
  inferSchema :: f Schema


instance InferSchema f => InferSchema (Compose (Tagged (t :: *)) f) where
  inferSchema = Compose $ Tagged inferSchema


instance (InferSchema l, InferSchema r) => InferSchema (HProduct l r) where
  inferSchema = HProduct inferSchema inferSchema


instance (Pattern x ~ Primitive t, KnownSymbol name) => InferSchema (Compose (Tagged name) (HIdentity x)) where
  inferSchema = Compose $ Tagged $ HIdentity $ Schema $ PrimSchema (symbolVal (Proxy @name))
