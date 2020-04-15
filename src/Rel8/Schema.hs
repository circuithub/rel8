{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema where

import Control.Applicative ( Const(..) )
import Control.Monad ( void )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Functor.Product ( Product(..) )
import Data.Functor.Sum ( Sum(..) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( I )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( hsequence )
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ((:~:))
import GHC.TypeLits
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Rel8.Column as Column
import Rel8.Null
import Rel8.Row
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


-- TODO Yuck. We really want to know if Rep f is isomorphic to unit (or f is isomorphic to Identity)
type family Simple (f :: Type -> Type) :: Constraint where
  Simple ((:~:) x) = ()
  Simple (Product (Const ()) (I (HIdentity t) Null)) = ()
  Simple _ = TypeError ('Text "Nested schema detected")


instance (KnownSymbol name, Simple f) => ColumnName (Product (Const (FieldName name ())) f) where
  columnName _ = symbolVal (Proxy @name)


table :: forall a. Table a => Schema a -> Opaleye.Table (Row a) (Row a)
table Schema{ tableName, schema = Columns columnNames } = Opaleye.Table tableName tableProperties
  where
    tableProperties = Opaleye.TableProperties writer view
      where
        writer = Opaleye.Writer $ Opaleye.PackMap \f values ->
          void $ hsequence $ htabulate @(Pattern a) \i ->
            let columnName = hindex columnNames i
                columnValues = fmap (flip hindex i . toColumns) values
            in Compose $ fmap Const $ Column.write f columnValues columnName

        view = Opaleye.View $ Row $ hmap Column.selectColumn columnNames
