{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table where

import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor.Constrained ( HConstrained(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable )
import Data.Indexed.Functor.Traversable ( HTraversable )
import Data.Kind ( Type )
import Data.Tagged.PolyKinded ( Tagged(..) )
import qualified GHC.Generics
import GHC.Generics ( Generic, Rep, M1(..), D, S, C, (:*:)(..), Meta(..), K1(..) )


-- | The class of "table-like" things.
class (HConstrained (Pattern a), HTraversable (Pattern a), HRepresentable (Pattern a)) => Table (a :: Type) where
  -- | A higher-kinded pattern functor for this table.
  --
  -- This is a bit like a generic encoding of 'a', but lifted to higher-kinded
  -- data.
  --
  -- This is an injective type family rather than a data family to aid
  -- generic deriving.
  type Pattern a = (r :: (Type -> Type) -> Type) | r -> a
  type Pattern a = Compose (Tagged a) (GPattern (Rep a))

  from :: a -> Pattern a Identity
  default from
    :: (Generic a, GTable (Rep a), Compose (Tagged a) (GPattern (Rep a)) ~ Pattern a)
    => a -> Pattern a Identity
  from = Compose . Tagged . gfrom . GHC.Generics.from

  to :: Pattern a Identity -> a
  default to
    :: (Generic a, GTable (Rep a), Compose (Tagged a) (GPattern (Rep a)) ~ Pattern a)
    => Pattern a Identity -> a
  to = GHC.Generics.to . gto . unTagged . getCompose


class GTable (f :: * -> *) where
  type GPattern f :: (* -> *) -> *

  gfrom :: f x -> GPattern f Identity
  gto :: GPattern f Identity -> f x


instance GTable f => GTable (M1 D c f) where
  type GPattern (M1 D c f) = GPattern f
  gfrom = gfrom . unM1
  gto = M1 . gto


instance GTable f => GTable (M1 C c f) where
  type GPattern (M1 C c f) = GPattern f
  gfrom = gfrom . unM1
  gto = M1 . gto


instance (GTable f, GTable g) => GTable (f :*: g) where
  type GPattern (f :*: g) = HProduct (GPattern f) (GPattern g)

  gfrom (a :*: b) = HProduct (gfrom a) (gfrom b)
  gto (HProduct a b) = gto a :*: gto b


instance GTable f => GTable (M1 S ('MetaSel ('Just name) x y z) f) where
  type GPattern (M1 S ('MetaSel ('Just name) x y z) f) =
    Compose (Tagged name) (GPattern f)

  gfrom = Compose . Tagged . gfrom . unM1
  gto = M1 . gto . unTagged . getCompose


instance GTable f => GTable (M1 S ('MetaSel 'Nothing x y z) f) where
  type GPattern (M1 S ('MetaSel 'Nothing x y z) f) =
    GPattern f

  gfrom = gfrom . unM1
  gto = M1 . gto


instance Table a => GTable (K1 i a) where
  type GPattern (K1 i a) = Pattern a
  gfrom = from . unK1
  gto = K1 . to



-- Base types are one column tables.


instance Table Bool where
  type Pattern Bool = HIdentity Bool
  from = coerce
  to = coerce


instance Table Int where
  type Pattern Int = HIdentity Int
  from = coerce
  to = coerce


instance (Table a, Table b) => Table (a, b)
