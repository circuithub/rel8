{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.OrdTable where

import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Kind
import Data.Proxy ( Proxy(..) )
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.ToField ( ToField )
import GHC.Generics
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import Rel8.Column
import Rel8.EqTable
import Rel8.Row
import Rel8.Table


-- | 'Table's that support a notion of equality.
class EqTable a => OrdTable a where
  (<=.) :: Row a -> Row a -> Row Bool
  default (<=.) :: GOrdTable (Rep a) (Schema a) => Row a -> Row a -> Row Bool
  Row a <=. Row b = gleq (Proxy @(Rep a ())) a b

  (<.) :: Row a -> Row a -> Row Bool
  default (<.) :: GOrdTable (Rep a) (Schema a) => Row a -> Row a -> Row Bool
  Row a <. Row b = gltq (Proxy @(Rep a ())) a b

  (>.) :: Row a -> Row a -> Row Bool
  default (>.) :: GOrdTable (Rep a) (Schema a) => Row a -> Row a -> Row Bool
  Row a >. Row b = ggtq (Proxy @(Rep a ())) a b

  (>=.) :: Row a -> Row a -> Row Bool
  default (>=.) :: GOrdTable (Rep a) (Schema a) => Row a -> Row a -> Row Bool
  Row a >=. Row b = ggeq (Proxy @(Rep a ())) a b


class GOrdTable (f :: Type -> Type) (g :: (Type -> Type) -> Type) where
  gleq :: Proxy (f x) -> g Column -> g Column -> Row Bool
  gltq :: Proxy (f x) -> g Column -> g Column -> Row Bool
  ggtq :: Proxy (f x) -> g Column -> g Column -> Row Bool
  ggeq :: Proxy (f x) -> g Column -> g Column -> Row Bool


instance GOrdTable f p => GOrdTable (M1 i c f) p where
  gleq = coerce (gleq @f @p)
  gltq = coerce (gltq @f @p)
  ggtq = coerce (ggtq @f @p)
  ggeq = coerce (ggeq @f @p)


instance (GOrdTable f x, GOrdTable g y) => GOrdTable (f :*: g) (HProduct x y) where
  gleq proxy (HProduct u v) (HProduct x y) =
    gleq @f (coerce proxy) u x &&. gleq @g (coerce proxy) v y

  gltq proxy (HProduct u v) (HProduct x y) =
    gltq @f (coerce proxy) u x &&. gltq @g (coerce proxy) v y

  ggtq proxy (HProduct u v) (HProduct x y) =
    ggtq @f (coerce proxy) u x &&. ggtq @g (coerce proxy) v y

  ggeq proxy (HProduct u v) (HProduct x y) =
    ggeq @f (coerce proxy) u x &&. ggeq @g (coerce proxy) v y


instance (OrdTable a, a ~ t, p ~ Schema t) => GOrdTable (K1 i a) (Compose (FieldName name) p) where
  gleq _ (Compose (FieldName x)) (Compose (FieldName y)) = (<=.) @a (Row x) (Row y)
  gltq _ (Compose (FieldName x)) (Compose (FieldName y)) = (<=.) @a (Row x) (Row y)
  ggtq _ (Compose (FieldName x)) (Compose (FieldName y)) = (<=.) @a (Row x) (Row y)
  ggeq _ (Compose (FieldName x)) (Compose (FieldName y)) = (<=.) @a (Row x) (Row y)


instance ( FromField a, ToField a ) => OrdTable ( PostgreSQLSimpleField a ) where
  (<=.) = coerce $ O.BinExpr (O.:<=)
  (<.) = coerce $ O.BinExpr (O.:<)
  (>.) = coerce $ O.BinExpr (O.:>)
  (>=.) = coerce $ O.BinExpr (O.:>=)


deriving via PostgreSQLSimpleField Bool instance OrdTable Bool
deriving via PostgreSQLSimpleField Int instance OrdTable Int
