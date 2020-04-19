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

module Rel8.EqTable where

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
import Rel8.Column hiding ((==.), (&&.), lit)
import qualified Rel8.Column
import Rel8.Row
import Rel8.Table


-- | 'Table's that support a notion of equality.
class Table a => EqTable a where
  (==.) :: Row a -> Row a -> Row Bool
  default (==.)
    :: GEqTable (Rep a) (Schema a)
    => Row a -> Row a -> Row Bool
  Row a ==. Row b = geq (Proxy @(Rep a ())) a b


class GEqTable (f :: Type -> Type) (g :: (Type -> Type) -> Type) where
  geq :: Proxy (f x) -> g Column -> g Column -> Row Bool


instance GEqTable f p => GEqTable (M1 i c f) p where
  geq proxy x y = geq @f (coerce proxy) x y


instance (GEqTable f x, GEqTable g y) => GEqTable (f :*: g) (HProduct x y) where
  geq proxy (HProduct u v) (HProduct x y) =
    geq @f (coerce proxy) u x &&. geq @g (coerce proxy) v y


instance (EqTable a, a ~ t, p ~ Schema t) => GEqTable (K1 i a) (Compose (FieldName name) p) where
  geq _ (Compose (FieldName x)) (Compose (FieldName y)) =
    (==.) @a (Row x) (Row y)


instance (FromField a, ToField a) => EqTable (PostgreSQLSimpleField a) where
  (==.) =
    coerce (Rel8.Column.==.)


deriving via PostgreSQLSimpleField Bool instance EqTable Bool
deriving via PostgreSQLSimpleField Int instance EqTable Int


instance EqTable a => EqTable (Maybe a) where
  x ==. y =
    maybe_
      (isNothing y)
      (\x' -> maybe_ (lit False) (x' ==.) y)
      x
