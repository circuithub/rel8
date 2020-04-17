{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table where

import Control.Applicative ( Const(..), getConst )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.Aeson ( Value )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy
import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Contravariant ( Op(..) )
import Data.Functor.FieldName ( FieldName(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Compose ( HCompose(..), I(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..), hzipWith )
import Data.Indexed.Functor.Traversable ( HTraversable(..), hsequence )
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Scientific ( Scientific )
import Data.Tagged.PolyKinded ( Tagged(..) )
import qualified Data.Text
import qualified Data.Text.Lazy
import Data.Time ( Day, LocalTime, TimeOfDay, UTCTime, ZonedTime )
import Data.UUID ( UUID )
import Database.PostgreSQL.Simple.FromField ( Conversion, Field, FromField, fromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import Database.PostgreSQL.Simple.HStore ( HStoreList, HStoreMap )
import Database.PostgreSQL.Simple.Time ( Date, LocalTimestamp, UTCTimestamp, ZonedTimestamp )
import Database.PostgreSQL.Simple.Types ( Null, Oid )
import qualified GHC.Generics
import GHC.Generics ( Generic, Rep, M1(..), D, S, C, (:*:)(..), Meta(..), K1(..) )
import Generics.OneLiner (ADT, Constraints, gfoldMap)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O


-- | The class of "table-like" things.
class (HTraversable (Schema a), HRepresentable (Schema a)) => Table (a :: Type) where
  -- | A higher-kinded pattern functor for this table.
  --
  -- This is a bit like a generic encoding of 'a', but lifted to higher-kinded
  -- data.
  --
  -- This is an injective type family rather than a data family to aid
  -- generic deriving.
  type Schema a :: (Type -> Type) -> Type
  type Schema a = Compose (Tagged a) (GSchema (Rep a))

  from :: a -> Schema a Identity
  default from
    :: (Generic a, GTable (Rep a), Compose (Tagged a) (GSchema (Rep a)) ~ Schema a)
    => a -> Schema a Identity
  from = Compose . Tagged . gfrom . GHC.Generics.from

  to :: Schema a Identity -> a
  default to
    :: (Generic a, GTable (Rep a), Compose (Tagged a) (GSchema (Rep a)) ~ Schema a)
    => Schema a Identity -> a
  to = GHC.Generics.to . gto . unTagged . getCompose


  decode :: Schema a (ReaderT Field (ReaderT (Maybe ByteString) Conversion))
  default decode
    :: (GTable (Rep a), Compose (Tagged a) (GSchema (Rep a)) ~ Schema a)
    => Schema a (ReaderT Field (ReaderT (Maybe ByteString) Conversion))
  decode = Compose $ Tagged $ gdecode (Proxy @(Rep a ()))

  encode :: Schema a (Op O.PrimExpr)
  default encode
    :: (GTable (Rep a), Compose (Tagged a) (GSchema (Rep a)) ~ Schema a)
    => Schema a (Op O.PrimExpr)
  encode = Compose $ Tagged $ gencode (Proxy @(Rep a ()))


class GTable (f :: * -> *) where
  type GSchema f :: (* -> *) -> *

  gfrom :: f x -> GSchema f Identity
  gto :: GSchema f Identity -> f x

  gdecode :: Proxy (f x) -> GSchema f (ReaderT Field (ReaderT (Maybe ByteString) Conversion))
  gencode :: Proxy (f x) -> GSchema f (Op O.PrimExpr)


instance GTable f => GTable (M1 D c f) where
  type GSchema (M1 D c f) = GSchema f
  gfrom = gfrom . unM1
  gto = M1 . gto

  gencode proxy = gencode (unM1 <$> proxy)
  gdecode proxy = gdecode (unM1 <$> proxy)


instance GTable f => GTable (M1 C c f) where
  type GSchema (M1 C c f) = GSchema f
  gfrom = gfrom . unM1
  gto = M1 . gto

  gencode proxy = gencode (unM1 <$> proxy)
  gdecode proxy = gdecode (unM1 <$> proxy)


instance (GTable f, GTable g) => GTable (f :*: g) where
  type GSchema (f :*: g) = HProduct (GSchema f) (GSchema g)

  gfrom (a :*: b) = HProduct (gfrom a) (gfrom b)
  gto (HProduct a b) = gto a :*: gto b

  gencode proxy = HProduct (gencode (fmap (\(x :*: _) -> x) proxy)) (gencode (fmap (\(_ :*: y) -> y) proxy))
  gdecode proxy = HProduct (gdecode (fmap (\(x :*: _) -> x) proxy)) (gdecode (fmap (\(_ :*: y) -> y) proxy))


instance GTable f => GTable (M1 S ('MetaSel ('Just name) x y z) f) where
  type GSchema (M1 S ('MetaSel ('Just name) x y z) f) =
    Compose (FieldName name) (GSchema f)

  gfrom = Compose . FieldName . gfrom . unM1
  gto = M1 . gto . unFieldName . getCompose

  gencode proxy = Compose $ FieldName $ gencode (unM1 <$> proxy)
  gdecode proxy = Compose $ FieldName $ gdecode (unM1 <$> proxy)


instance GTable f => GTable (M1 S ('MetaSel 'Nothing x y z) f) where
  type GSchema (M1 S ('MetaSel 'Nothing x y z) f) =
    GSchema f

  gfrom = gfrom . unM1
  gto = M1 . gto

  gencode proxy = gencode (unM1 <$> proxy)
  gdecode proxy = gdecode (unM1 <$> proxy)


instance Table a => GTable (K1 i a) where
  type GSchema (K1 i a) = Schema a
  gfrom = from . unK1
  gto = K1 . to

  gencode _ = encode @a
  gdecode _ = decode @a



-- Base types are one column tables.


newtype PostgreSQLSimpleField a =
  PostgreSQLSimpleField a


instance FromField a => Table (PostgreSQLSimpleField a) where
  type Schema (PostgreSQLSimpleField a) = HIdentity a
  from = coerce
  to = coerce
  decode = coerce $ fromField @a
  encode = undefined


deriving via (PostgreSQLSimpleField Bool) instance Table Bool
deriving via (PostgreSQLSimpleField Char) instance Table Char
deriving via (PostgreSQLSimpleField Double) instance Table Double
deriving via (PostgreSQLSimpleField Float) instance Table Float
deriving via (PostgreSQLSimpleField Int) instance Table Int
deriving via (PostgreSQLSimpleField Int16) instance Table Int16
deriving via (PostgreSQLSimpleField Int32) instance Table Int32
deriving via (PostgreSQLSimpleField Int64) instance Table Int64
deriving via (PostgreSQLSimpleField Integer) instance Table Integer
deriving via (PostgreSQLSimpleField ()) instance Table ()
deriving via (PostgreSQLSimpleField Data.ByteString.ByteString) instance Table Data.ByteString.ByteString
deriving via (PostgreSQLSimpleField Data.ByteString.Lazy.ByteString) instance Table Data.ByteString.Lazy.ByteString
deriving via (PostgreSQLSimpleField Scientific) instance Table Scientific
deriving via (PostgreSQLSimpleField Data.Text.Text) instance Table Data.Text.Text
deriving via (PostgreSQLSimpleField UTCTime) instance Table UTCTime
deriving via (PostgreSQLSimpleField Value) instance Table Value
deriving via (PostgreSQLSimpleField Data.Text.Lazy.Text) instance Table Data.Text.Lazy.Text
deriving via (PostgreSQLSimpleField Oid) instance Table Oid
deriving via (PostgreSQLSimpleField ZonedTime) instance Table ZonedTime
deriving via (PostgreSQLSimpleField LocalTime) instance Table LocalTime
deriving via (PostgreSQLSimpleField TimeOfDay) instance Table TimeOfDay
deriving via (PostgreSQLSimpleField Day) instance Table Day
deriving via (PostgreSQLSimpleField UUID) instance Table UUID
deriving via (PostgreSQLSimpleField Date) instance Table Date
deriving via (PostgreSQLSimpleField ZonedTimestamp) instance Table ZonedTimestamp
deriving via (PostgreSQLSimpleField UTCTimestamp) instance Table UTCTimestamp
deriving via (PostgreSQLSimpleField LocalTimestamp) instance Table LocalTimestamp
deriving via (PostgreSQLSimpleField Null) instance Table Null
deriving via (PostgreSQLSimpleField HStoreMap) instance Table HStoreMap
deriving via (PostgreSQLSimpleField HStoreList) instance Table HStoreList
deriving via (PostgreSQLSimpleField String) instance Table String
deriving via (PostgreSQLSimpleField Rational) instance Table Rational


instance (Table a, Table b) => Table (a, b)
instance (Table a, Table b, Table c) => Table (a, b, c)
instance (Table a, Table b, Table c, Table d) => Table (a, b, c, d)
instance (Table a, Table b, Table c, Table d, Table e) => Table (a, b, c, d, e)
instance (Table a, Table b, Table c, Table d, Table e, Table f) => Table (a, b, c, d, e, f)
instance (Table a, Table b, Table c, Table d, Table e, Table f, Table g) => Table (a, b, c, d, e, f, g)



-- | This @newtype@ can be used to derive 'Table' instances for types that are
-- stored in the database as a single text column, using Haskell's 'Show' and
-- 'Read' type classes as a serialization format.
newtype ReadShowColumn a = ReadShowColumn a


instance (Read a, Show a) => Table (ReadShowColumn a) where
  type Schema (ReadShowColumn a) = HIdentity a
  to = coerce
  from = coerce

  encode = HIdentity $ coerce (O.ConstExpr . O.StringLit . show @a)
  decode = HIdentity $ coerce $ \x -> fmap (read @a) . fromField @String x


newtype CompositeColumn a = CompositeColumn a


instance (ADT a, Constraints a Table, FromField a) => Table (CompositeColumn a) where
  type Schema (CompositeColumn a) = HIdentity a
  to = coerce
  from = coerce

  encode = HIdentity $ Op $ catPrimExprs . gfoldMap @Table primExprs
    where
      catPrimExprs = O.FunExpr ""

      primExprs :: forall s. Table s => s -> [O.PrimExpr]
      primExprs s =
        getConst $ hsequence $ hzipWith (\(Op encoder) (Identity x) -> Compose $ Const [ encoder x ]) (encode @s) (from s)


  -- TODO We will have to write some kind of parser here. postgresql-simple doesn't
  -- have good support for composite types. For now, force the user to do this.
  decode = coerce $ fromField @a



rowParser :: Table a => RowParser a
rowParser = rowParser'
  where
    rowParser' :: forall a. Table a => RowParser a
    rowParser' = to <$> htraverse (coerce fieldWith) (decode @a)


instance Table a => Table (Maybe a) where
  type Schema (Maybe a) =
    Compose (Tagged (Maybe a)) (HProduct (HIdentity Bool) (HCompose (Schema a) Maybe))

  from =
    maybe
      (Compose $ Tagged $ HProduct (coerce True) $ htabulate \(I _) -> Identity Nothing)
      (\x -> Compose $ Tagged $ HProduct (coerce False) $ htabulate \(I i) -> Just <$> hindex (from x) i)

  to (Compose (Tagged (HProduct _ (HCompose x)))) = do
    to <$> htraverse (\(Compose y) -> Identity <$> runIdentity y) x

  encode =
    Compose $ Tagged $ HProduct (encode @Bool) $ HCompose $ hmap (\(Op f) -> Compose $ Op $ maybe (O.ConstExpr O.NullLit) f) $ encode @a

  decode = Compose $ Tagged $ HProduct (decode @Bool) $ HCompose $ hmap (\f -> Compose (nullIsNothing f)) $ decode @a
    where
      nullIsNothing parser = ReaderT \field -> ReaderT (maybe (pure Nothing) (runReaderT (runReaderT (Just <$> parser) field) . Just))
