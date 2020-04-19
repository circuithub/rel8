{-# language AllowAmbiguousTypes #-}
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
{-# language UndecidableInstances #-}

-- | This module describes the 'EqTable' class.

module Rel8.EqTable ( EqTable(..) ) where

import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Text ( Text )
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.ToField ( ToField )
import GHC.Generics
import qualified Rel8.Column
import Rel8.Column hiding ( (&&.), (==.), lit )
import Rel8.Row
import Rel8.Table


-- | 'Table's that support a notion of equality.
class Table a => EqTable a where
  (==.) :: Row a -> Row a -> Row Bool

  default (==.)
    :: (EqTable (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Row a -> Row a -> Row Bool
  x ==. y = (==.) @(Rep a ()) (coerceRow x) (coerceRow y)


instance EqTable (f a) => EqTable (M1 D c f a) where
  x ==. y =
    (==.) @(f a) (coerceRow x) (coerceRow y)


instance EqTable (f a) => EqTable (M1 C c f a) where
  x ==. y =
    (==.) @(f a) (coerceRow x) (coerceRow y)


instance EqTable (f a) => EqTable (M1 S ('MetaSel ('Just fieldName) x y z) f a) where
  Row (Compose (FieldName x)) ==. Row (Compose (FieldName y)) =
    (==.) @(f a) (Row x) (Row y)


instance (EqTable (l a), EqTable (r a)) => EqTable ((l :*: r) a) where
  Row (HProduct a b) ==. Row (HProduct x y) =
    (Row @(l a) a ==. Row x) &&. (Row @(r a) b ==. Row y)


instance EqTable a => EqTable (K1 i a x) where
  x ==. y =
    (==.) @a (coerceRow x) (coerceRow y)


instance (FromField a, ToField a) => EqTable (PostgreSQLSimpleField a) where
  (==.) =
    coerce (Rel8.Column.==.)


deriving via PostgreSQLSimpleField Bool instance EqTable Bool


deriving via PostgreSQLSimpleField Int instance EqTable Int


deriving via PostgreSQLSimpleField Text instance EqTable Text


instance (Read a, Show a) => EqTable (ReadShowColumn a) where
  (==.) =
    coerce (Rel8.Column.==.)


instance EqTable a => EqTable (Maybe a) where
  x ==. y =
    maybe_
      (isNothing y)
      (\x' -> maybe_ (lit False) (x' ==.) y)
      x
