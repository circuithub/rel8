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

module Rel8.OrdTable ( OrdTable( lte, lt, gt, gte ), (<=.) ) where

-- base
import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose( Compose ) )
import GHC.Generics

-- postgresql-simple
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.ToField ( ToField )

-- rel8
import Data.Functor.FieldName ( FieldName( FieldName ) )
import Data.Indexed.Functor.Identity ( HIdentity( HIdentity ) )
import Data.Indexed.Functor.Product ( HProduct( HProduct ) )
import qualified Rel8.Column ( (<=.) )
import Rel8.Column hiding ( (&&.), (<.), (<=.), (>.), (>=.) )
import Rel8.EqTable ( EqTable )
import Rel8.Row ( Row( Row ), (&&.) )
import Rel8.Table ( PostgreSQLSimpleField, ReadShowColumn, Schema )

-- text
import Data.Text ( Text )


(<=.) :: forall a. OrdTable a => Row a -> Row a -> Row Bool
Row x <=. Row y = lte @a x y


-- | 'Table's that support a notion of equality.
class EqTable a => OrdTable a where
  lte :: Schema a Column -> Schema a Column -> Row Bool
  lt  :: Schema a Column -> Schema a Column -> Row Bool
  gt  :: Schema a Column -> Schema a Column -> Row Bool
  gte :: Schema a Column -> Schema a Column -> Row Bool

  default lte
    :: (OrdTable (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a Column -> Schema a Column -> Row Bool
  lte = lte @(Rep a ())

  default lt
    :: (OrdTable (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a Column -> Schema a Column -> Row Bool
  lt = lt @(Rep a ())

  default gte
    :: (OrdTable (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a Column -> Schema a Column -> Row Bool
  gte = gte @(Rep a ())

  default gt
    :: (OrdTable (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a Column -> Schema a Column -> Row Bool
  gt = gt @(Rep a ())


instance OrdTable (f a) => OrdTable (M1 D c f a) where
  lte = lte @(f a)
  lt = lt @(f a)
  gt = gt @(f a)
  gte = gte @(f a)


instance OrdTable (f a) => OrdTable (M1 C c f a) where
  lte = lte @(f a)
  lt = lt @(f a)
  gt = gt @(f a)
  gte = gte @(f a)


instance (OrdTable (l a), OrdTable (r a)) => OrdTable ((l :*: r) a) where
  lte (HProduct a b) (HProduct x y) = lte @(l a) a x &&. lte @(r a) b y
  lt (HProduct a b) (HProduct x y) = lt @(l a) a x &&. lt @(r a) b y
  gt (HProduct a b) (HProduct x y) = gt @(l a) a x &&. gt @(r a) b y
  gte (HProduct a b) (HProduct x y) = gte @(l a) a x &&. gte @(r a) b y


instance OrdTable (f a) => OrdTable (M1 S ('MetaSel ('Just fieldName) x y z) f a) where
  lte (Compose (FieldName x)) (Compose (FieldName y)) = lte @(f a) x y
  lt (Compose (FieldName x)) (Compose (FieldName y)) = lt @(f a) x y
  gt (Compose (FieldName x)) (Compose (FieldName y)) = gt @(f a) x y
  gte (Compose (FieldName x)) (Compose (FieldName y)) = gte @(f a) x y


instance ( FromField a, ToField a ) => OrdTable ( PostgreSQLSimpleField a ) where
  lte x y = coerce (Rel8.Column.<=.) (Row @(PostgreSQLSimpleField a) x) (Row @(PostgreSQLSimpleField a) y)
  lt x y = coerce (Rel8.Column.<=.) (Row @(PostgreSQLSimpleField a) x) (Row @(PostgreSQLSimpleField a) y)
  gt x y = coerce (Rel8.Column.<=.) (Row @(PostgreSQLSimpleField a) x) (Row @(PostgreSQLSimpleField a) y)
  gte x y = coerce (Rel8.Column.<=.) (Row @(PostgreSQLSimpleField a) x) (Row @(PostgreSQLSimpleField a) y)


deriving via PostgreSQLSimpleField Bool instance OrdTable Bool


deriving via PostgreSQLSimpleField Int instance OrdTable Int


deriving via PostgreSQLSimpleField Text instance OrdTable Text


instance (Read a, Show a) => OrdTable (ReadShowColumn a) where
  lte x y = coerce (Rel8.Column.<=.) (Row @(ReadShowColumn a) x) (Row @(ReadShowColumn a) y)
  lt x y = coerce (Rel8.Column.<=.) (Row @(ReadShowColumn a) x) (Row @(ReadShowColumn a) y)
  gt x y = coerce (Rel8.Column.<=.) (Row @(ReadShowColumn a) x) (Row @(ReadShowColumn a) y)
  gte x y = coerce (Rel8.Column.<=.) (Row @(ReadShowColumn a) x) (Row @(ReadShowColumn a) y)
