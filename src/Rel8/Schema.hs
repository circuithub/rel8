{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Schema
  ( TableSchema( TableSchema, tableName, schema )
  , table
  , genericColumns
  ) where

-- base
import Control.Applicative ( Const( Const ) )
import Control.Monad ( void )
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics ( (:*:), C, D, K1, M1, Meta( MetaSel ), Rep, S )
import GHC.TypeLits ( KnownSymbol, symbolVal )

-- opaleye
import Opaleye.Internal.PackMap ( PackMap( PackMap ) )
import Opaleye.Internal.Table
  ( TableProperties( TableProperties )
  , View( View )
  , Writer( Writer )
  )
import qualified Opaleye.Internal.Table as Opaleye ( Table( Table ) )

-- rel8
import Data.Functor.FieldName ( FieldName( FieldName ) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose( HCompose ) )
import Data.Indexed.Functor.Identity ( HIdentity( HIdentity ) )
import Data.Indexed.Functor.Product ( HProduct( HProduct ) )
import Data.Indexed.Functor.Representable ( hindex, htabulate )
import Data.Indexed.Functor.Traversable ( hsequence )
import Rel8.Column
  ( ColumnSchema
  , concreteColumn
  , derivedColumn
  , isNull
  , select
  , write
  )
import Rel8.Row ( Row( Row ), toColumns )
import Rel8.Table ( Schema, Table )


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
instance {-# overlappable #-} (KnownSymbol name, Table a, Schema a ~ HIdentity something) => InferColumns (M1 S ('MetaSel ('Just name) x y z) (K1 i a) b) where
  inferColumns = Columns $ Compose $ FieldName $ HIdentity $ concreteColumn $ symbolVal $ Proxy @name


instance {-# overlapping #-} (KnownSymbol name, Table a, Schema a ~ HIdentity something) => InferColumns (M1 S ('MetaSel ('Just name) x y z) (K1 i (Maybe a)) b) where
  inferColumns = Columns $ Compose $ FieldName $ HProduct (HIdentity $ derivedColumn @() isNull (symbolVal @name Proxy)) $ HCompose $ HIdentity $ Compose $ concreteColumn (symbolVal @name Proxy)


table :: forall a. Table a => TableSchema a -> Opaleye.Table (Row a) (Row a)
table TableSchema{ tableName, schema = Columns columnSchemas } = Opaleye.Table tableName tableProperties
  where
    tableProperties = TableProperties writer view
      where
        writer = Writer $ PackMap \f values ->
          void $ hsequence $ htabulate @(Schema a) \i ->
            let columnValues = fmap (flip hindex i . toColumns) values
            in
            case write (hindex columnSchemas i) of
              Writer (PackMap g) ->
                Compose $ Const <$> g f columnValues

        view = View $ Row $ hmap select columnSchemas
