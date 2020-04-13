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
import Data.Functor.Contravariant ( Op(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..), hzipWith )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Singletons.Prelude ( If )
import Data.Tagged.PolyKinded ( Tagged(..) )
import GHC.Records.Compat ( HasField(..) )
import GHC.TypeLits ( Symbol )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Table ( Table(..) )


-- | Typed expressions.
newtype Expr a =
  Expr (Pattern a (Const Opaleye.PrimExpr))


toPrimExprs :: Expr a -> Pattern a (Const Opaleye.PrimExpr)
toPrimExprs (Expr x) = x


type family HasName (name :: Symbol) f :: Bool where
  HasName name (Compose (FieldName name) y) = 'True
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


instance HProductHasField f g r (WhichSide name f g r) name i => HasField name (HProduct f g i) r where
  hasField =
    hproductHasField (Proxy @(WhichSide name f g r)) (Proxy @name)


instance (name ~ name', f ~ g) => HasField name (Compose (FieldName name') f i) (g i) where
  hasField (Compose (FieldName x)) = (setter, getter) where
    setter = Compose . FieldName
    getter = x


instance HasField name (g i) r => HasField name (Compose (Tagged (x :: Type)) g i) r where
  hasField (Compose (Tagged x)) = (setter, getter) where
    setter = Compose . Tagged . fst (hasField @name x)
    getter = snd $ hasField @name x


instance (HasField name a r, HasField name (Pattern a (Const Opaleye.PrimExpr)) (Pattern r (Const Opaleye.PrimExpr))) => HasField (name :: Symbol) (Expr a) (Expr r) where
  hasField (Expr x) = (setter, getter) where
    setter (Expr r) = Expr $ fst (hasField @name x) r
    getter = Expr $ snd $ hasField @name x


fst_ :: Expr (a, b) -> Expr a
fst_ (Expr (Compose (Tagged (HProduct x _)))) = Expr x


snd_ :: Expr (a, b) -> Expr b
snd_ (Expr (Compose (Tagged (HProduct _ y)))) = Expr y


isNothing :: Table a => Expr (Maybe a) -> Expr Bool
isNothing = maybe_ (lit True) (const $ lit False)


maybe_ :: (Table a, Table b) => Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
maybe_ (Expr def) f (Expr (Compose (Tagged (HProduct (HIdentity isNull) (HCompose row))))) = Expr $ htabulate \i ->
  Const $
  Opaleye.CaseExpr
    [(getConst isNull, getConst (hindex def i))]
    (getConst (hindex (toPrimExprs (f (Expr $ hmap (\(Compose (Const x)) -> Const x) row))) i))


lit :: forall a. Table a => a -> Expr a
lit = Expr . hzipWith (\(Op f) (Identity x) -> Const $ Opaleye.ConstExpr $ f x) (encode @a) . from
