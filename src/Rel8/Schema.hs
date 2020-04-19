{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Schema where

import Control.Applicative ( Const(..) )
import Control.Monad ( void )
import Data.Coerce ( coerce )
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


genericColumns :: forall a. GInferColumns (Rep a) (Schema a) => Columns a
genericColumns = Columns (ginferColumns @(Rep a) @(Schema a) @() Proxy)


class Table a => InferColumns a where
  inferColumns :: Columns a
  default inferColumns :: GInferColumns (Rep a) (Schema a) => Columns a
  inferColumns = Columns (ginferColumns @(Rep a) @(Schema a) @() Proxy)


class GInferColumns f g where
  ginferColumns :: Proxy (f x) -> g ColumnSchema


instance GInferColumns f g => GInferColumns (M1 i c f) g where
  ginferColumns = coerce (ginferColumns @f @g)


instance (GInferColumns f i, GInferColumns h g) => GInferColumns (f :*: h) (HProduct i g) where
  ginferColumns = HProduct <$> coerce (ginferColumns @f @i) <*> coerce (ginferColumns @h @g)


instance (KnownSymbol name, s ~ t) => GInferColumns (K1 i s) (Compose (FieldName name) (HIdentity t)) where
  ginferColumns _ = Compose $ FieldName $ HIdentity $ concreteColumn (symbolVal @name Proxy)


instance (KnownSymbol name, x ~ t) => GInferColumns (K1 i (Maybe x)) (Compose (FieldName name) (HProduct (HIdentity Bool) (HCompose (HIdentity x) Maybe))) where
  ginferColumns _ =
    Compose $ FieldName $ HProduct (HIdentity $ derivedColumn @() isNull (symbolVal @name Proxy)) $ HCompose $ HIdentity $ Compose $ concreteColumn (symbolVal @name Proxy)



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
