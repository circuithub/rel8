{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema where

import Control.Applicative ( Const(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Functor.Product ( Product(..) )
import Data.Functor.Sum ( Sum(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ((:~:))
import GHC.TypeLits
import Rel8.Table ( Table(..) )


data Schema a =
  Schema
    { tableName :: String
    , schema :: Columns a
    }


newtype Columns a = Columns (Pattern a (Const String))


genericColumns :: (ColumnName (HRep (Pattern a)), Table a) => Columns a
genericColumns = Columns $ htabulate \i -> Const $ columnName i


class ColumnName f where
  columnName :: f a -> String

instance ColumnName f => ColumnName (Product (Const ()) f) where
  columnName (Pair _ r) = columnName r


instance (ColumnName f, ColumnName g) => ColumnName (Sum f g) where
  columnName (InL x) = columnName x
  columnName (InR x) = columnName x


type family Simple (f :: Type -> Type) :: Constraint where
  Simple ((:~:) x) = ()
  Simple _ = TypeError ('Text "Nested schema detected")


instance (KnownSymbol name, Simple f) => ColumnName (Product (Const (FieldName name ())) f) where
  columnName _ = symbolVal (Proxy @name)
