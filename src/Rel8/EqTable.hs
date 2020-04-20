{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

-- | This module describes the 'EqTable' class.

module Rel8.EqTable ( EqTable( eq ), (==.) ) where

-- base
import Data.Coerce ( coerce )
import Data.Int ( Int32, Int64 )
import Data.Functor.Compose ( Compose( Compose ) )
import GHC.Generics ( (:*:), C, D, K1, M1, Meta( MetaSel ), Rep, S )

-- postgresql-simple
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.ToField ( ToField )

-- rel8
import Data.Functor.FieldName ( FieldName( FieldName ) )
import Data.Indexed.Functor.Identity ( HIdentity( HIdentity ) )
import Data.Indexed.Functor.Product ( HProduct( HProduct ) )
import qualified Rel8.Column
import Rel8.Column hiding ( (&&.), (==.), lit )
import Rel8.Row ( Row( Row ), (&&.), isNothing, lit, maybe_ )
import Rel8.Table ( PostgreSQLSimpleTable, ReadShowTable, Schema, Table )

-- text
import Data.Text ( Text )


infix 4 ==.
(==.) :: forall a. EqTable a => Row a -> Row a -> Row Bool
Row x ==. Row y = eq @a x y


-- | 'Table's that support a notion of equality.
class Table a => EqTable a where
  eq :: Schema a Column -> Schema a Column -> Row Bool

  default eq
    :: (EqTable (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a Column -> Schema a Column -> Row Bool
  eq = eq @(Rep a ())


instance EqTable (f a) => EqTable (M1 D c f a) where
  eq = eq @(f a)


instance EqTable (f a) => EqTable (M1 C c f a) where
  eq = eq @(f a)


instance EqTable (f a) => EqTable (M1 S ('MetaSel ('Just fieldName) x y z) f a) where
  eq (Compose (FieldName x)) (Compose (FieldName y)) =
    eq @(f a) x y


instance (EqTable (l a), EqTable (r a)) => EqTable ((l :*: r) a) where
  eq (HProduct a b) (HProduct x y) =
    eq @(l a) a x &&. eq @(r a) b y


instance EqTable a => EqTable (K1 i a x) where
  eq = eq @a


instance (FromField a, ToField a) => EqTable (PostgreSQLSimpleTable a) where
  eq x y =
    coerce
      (Rel8.Column.==.)
      (Row @(PostgreSQLSimpleTable a) x)
      (Row @(PostgreSQLSimpleTable a) y)


deriving via PostgreSQLSimpleTable Bool instance EqTable Bool


deriving via PostgreSQLSimpleTable Int instance EqTable Int


deriving via PostgreSQLSimpleTable Int32 instance EqTable Int32


deriving via PostgreSQLSimpleTable Int64 instance EqTable Int64


deriving via PostgreSQLSimpleTable Text instance EqTable Text


instance (Read a, Show a) => EqTable (ReadShowTable a) where
  eq x y =
    coerce
      (Rel8.Column.==.)
      (Row @(ReadShowTable a) x)
      (Row @(ReadShowTable a) y)


instance EqTable a => EqTable (Maybe a) where
  eq x y =
    maybe_
      (isNothing (Row @(Maybe a) y))
      (\x' -> maybe_ (lit False) (x' ==.) (Row @(Maybe a) y))
      (Row @(Maybe a) x)
