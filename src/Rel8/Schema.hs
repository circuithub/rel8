{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Schema where

import Control.Applicative ( Const(..) )
import Control.Monad ( void )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( hsequence )
import Data.Proxy ( Proxy(..) )
import GHC.Generics
import GHC.TypeLits
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import Rel8.Column ( ColumnSchema(..), concreteColumn, derivedColumn, isNull )
import Rel8.Row
import Rel8.Table ( Table(..) )


data TableSchema a =
  TableSchema
    { tableName :: String
    , schema :: Columns a
    }


newtype Columns a =
  Columns (Schema a ColumnSchema)


fromColumns :: Columns a -> Schema a ColumnSchema
fromColumns (Columns x) = x


coerceColumns :: Schema a ~ Schema b => Columns a -> Columns b
coerceColumns (Columns x) =
  Columns x


genericColumns :: forall a. Schema a ~ Schema (Rep a ()) => InferColumns (Rep a ()) => Columns a
genericColumns = coerceColumns (inferColumns @(Rep a ()))


class Table a => InferColumns a where
  inferColumns :: Columns a
  default inferColumns :: Schema a ~ Schema (Rep a ()) => InferColumns (Rep a ()) => Columns a
  inferColumns = coerceColumns (inferColumns @(Rep a ()))


instance InferColumns (f a) => InferColumns (M1 D c f a) where
  inferColumns =
    coerceColumns (inferColumns @(f a))


instance InferColumns (f a) => InferColumns (M1 C c f a) where
  inferColumns =
    coerceColumns (inferColumns @(f a))


instance (InferColumns (f a), InferColumns (g a)) => InferColumns ((f :*: g) a) where
  inferColumns =
    Columns $ HProduct (fromColumns (inferColumns @(f a))) (fromColumns (inferColumns @(g a)))

-- TODO
instance KnownSymbol name => InferColumns (M1 S ('MetaSel ('Just name) x y z) (K1 i Int) a) where
  inferColumns = Columns $ Compose $ FieldName $ HIdentity $ concreteColumn $ symbolVal $ Proxy @name

instance KnownSymbol name => InferColumns (M1 S ('MetaSel ('Just name) x y z) (K1 i Bool) a) where
  inferColumns = Columns $ Compose $ FieldName $ HIdentity $ concreteColumn $ symbolVal $ Proxy @name

instance KnownSymbol name => InferColumns (M1 S ('MetaSel ('Just name) x y z) (K1 i String) a) where
  inferColumns = Columns $ Compose $ FieldName $ HIdentity $ concreteColumn $ symbolVal $ Proxy @name

instance KnownSymbol name => InferColumns (M1 S ('MetaSel ('Just name) x y z) (K1 i (Maybe Int)) a) where
  inferColumns = Columns $ Compose $ FieldName $ HProduct (HIdentity $ derivedColumn @() isNull (symbolVal @name Proxy)) $ HCompose $ HIdentity $ Compose $ concreteColumn (symbolVal @name Proxy)

instance KnownSymbol name => InferColumns (M1 S ('MetaSel ('Just name) x y z) (K1 i (Maybe String)) a) where
  inferColumns = Columns $ Compose $ FieldName $ HProduct (HIdentity $ derivedColumn @() isNull (symbolVal @name Proxy)) $ HCompose $ HIdentity $ Compose $ concreteColumn (symbolVal @name Proxy)


table :: forall a. Table a => TableSchema a -> Opaleye.Table (Row a) (Row a)
table TableSchema{ tableName, schema = Columns columnSchemas } = Opaleye.Table tableName tableProperties
  where
    tableProperties = Opaleye.TableProperties writer view
      where
        writer = Opaleye.Writer $ Opaleye.PackMap \f values ->
          void $ hsequence $ htabulate @(Schema a) \i ->
            let columnValues = fmap (flip hindex i . toColumns) values
            in
            case write (hindex columnSchemas i) of
              Opaleye.Writer (Opaleye.PackMap g) ->
                Compose $ fmap Const $ g f columnValues

        view = Opaleye.View $ Row $ hmap select columnSchemas
