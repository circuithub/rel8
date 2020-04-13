{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Rel8.Expr where

import Control.Applicative ( Const(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )
import Data.Functor.Sum ( Sum(..) )
import Data.Indexed.Functor.Identity ( HIdentity )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Singletons ( TyCon1 )
import Data.Singletons.Prelude ( Fmap, If )
import Data.Singletons.Prelude.Foldable (Asum)
import Data.Singletons.Prelude.Maybe ( FromJust )
import Data.Tagged.PolyKinded ( Tagged(..) )
import Data.Type.Equality ( (:~:)(..), TestEquality(..) )
import GHC.Records.Compat ( HasField(..) )
import GHC.TypeLits ( Symbol )
import qualified Rel8.SQL as SQL
import Rel8.Table ( Table(..) )


-- | Typed expressions.
newtype Expr a =
  Expr (Pattern a (Const SQL.Expr))


-- Try and find the path to a given named field.
type family FindName k (name :: Symbol) f :: Maybe k where
  FindName (Product (Const ()) f r) name (Compose (Tagged (t :: *)) g) =
    Fmap (TyCon1 ('Pair ('Const '()))) (FindName (f r) name g)

  FindName (Product (Const ()) ((:~:) x) x) name (Compose (Tagged name) (HIdentity r)) =
    'Just ('Pair ('Const '()) 'Refl)

  FindName (Sum (Product (Const ()) ((:~:) r)) _ r) name (HProduct (Compose (Tagged name) (HIdentity r)) _) =
    'Just ('InL ('Pair ('Const '()) 'Refl))

  FindName (Sum l r x) name (HProduct f g) =
    Asum
      '[ Fmap (TyCon1 'InL ) (FindName (l x) name f)
       , Fmap (TyCon1 'InR) (FindName (r x) name g)
       ]

  FindName _ _ _ =
    'Nothing


type family HasName (name :: Symbol) f :: Bool where
  HasName name (Compose (Tagged name) y) = 'True
  HasName name _ = 'False


type family WhichSide (name :: Symbol) f g r :: Side where
  WhichSide name f g r = If (HasName name f) 'L 'R


data Side = L | R


class HProductHasField f g r (side :: Side) (name :: Symbol) i | name f g i -> r where
  hproductHasField :: Proxy side -> Proxy name -> HProduct f g i -> (r -> HProduct f g i, r)


instance HasField name (f i) r => HProductHasField f g r 'L name i where
  hproductHasField Proxy Proxy (HProduct x y) = (setter, getter) where
    setter r = HProduct (fst (hasField @name x) r) y
    getter = snd (hasField @name x)


instance HasField name (g i) r => HProductHasField f g r 'R name i where
  hproductHasField Proxy Proxy (HProduct x y) = (setter, getter) where
    setter r = HProduct x (fst (hasField @name y) r)
    getter = snd (hasField @name y)


instance HProductHasField f g r (WhichSide name f g r) name i => HasField name (HProduct f g i) r


instance (name ~ name', f ~ g) => HasField name (Compose (Tagged name') f i) (g i) where
  hasField (Compose (Tagged x)) = (setter, getter) where
    setter = Compose . Tagged
    getter = x


instance HasField name (g i) r => HasField name (Compose (Tagged (x :: Type)) g i) r where
  hasField (Compose (Tagged x)) = (setter, getter) where
    setter = Compose . Tagged . fst (hasField @name x)
    getter = snd $ hasField @name x


instance (HasField name a r, HasField name (Pattern a (Const SQL.Expr)) (Pattern r (Const SQL.Expr))) => HasField (name :: Symbol) (Expr a) (Expr r) where
  hasField (Expr x) = (setter, getter) where
    setter (Expr r) = Expr $ fst (hasField @name x) r
    getter = Expr $ snd $ hasField @name x


fst_ :: Expr (a, b) -> Expr a
fst_ (Expr (Compose (Tagged (HProduct x _)))) = Expr x


snd_ :: Expr (a, b) -> Expr b
snd_ (Expr (Compose (Tagged (HProduct _ y)))) = Expr y


isNothing :: Expr (Maybe a) -> Expr Bool
isNothing = maybe_ (lit True) (const $ lit False)


maybe_ :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
maybe_ = undefined


class DBType a where
  lit :: a -> Expr a


instance DBType Bool where
  lit = undefined
