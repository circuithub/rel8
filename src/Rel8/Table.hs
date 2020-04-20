{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, Value )

-- base
import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Type )
import Data.Typeable ( Typeable )
import qualified GHC.Generics
import GHC.Generics ( (:*:)(..), C, D, Generic, K1(..), M1(..), Meta(..), Rep, S )

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy

-- one-liner
import Generics.OneLiner ( ADT, Constraints )

-- postgresql-simple
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser )
import Database.PostgreSQL.Simple.HStore ( HStoreList, HStoreMap )
import Database.PostgreSQL.Simple.Time ( Date, LocalTimestamp, UTCTimestamp, ZonedTimestamp )
import Database.PostgreSQL.Simple.ToField ( ToField )
import Database.PostgreSQL.Simple.Types ( Null, Oid )

-- rel8
import Data.Functor.FieldName ( FieldName(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Compose ( HCompose(..), I(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Rel8.Column

-- scientific
import Data.Scientific ( Scientific )

-- text
import qualified Data.Text
import qualified Data.Text.Lazy

-- time
import Data.Time ( Day, LocalTime, TimeOfDay, UTCTime, ZonedTime )

-- uuid
import Data.UUID ( UUID )


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
  type Schema a = Schema (Rep a ())

  from :: a -> Schema a Identity
  default from
    :: (Generic a, Table (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => a -> Schema a Identity
  from = from @(Rep a ()) . GHC.Generics.from

  to :: Schema a Identity -> a
  default to
    :: (Generic a, Table (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a Identity -> a
  to = GHC.Generics.to @_ @() . to

  decode :: Schema a ColumnDecoder
  default decode
    :: (Table (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a ColumnDecoder
  decode = decode @(Rep a ())

  encode :: Schema a ColumnEncoder
  default encode
    :: (Table (Rep a ()), Schema (Rep a ()) ~ Schema a)
    => Schema a ColumnEncoder
  encode = encode @(Rep a ())


instance Table (f a) => Table (M1 D c f a) where
  type Schema (M1 D c f a) = Schema (f a)

  from = from . unM1
  to = M1 . to

  encode = encode @(f a)
  decode = decode @(f a)


instance Table (f a) => Table (M1 C c f a) where
  type Schema (M1 C c f a) = Schema (f a)
  from = from . unM1
  to = M1 . to

  encode = encode @(f a)
  decode = decode @(f a)


instance (Table (f a), Table (g a)) => Table ((f :*: g) a) where
  type Schema ((f :*: g) a) = HProduct (Schema (f a)) (Schema (g a))

  from (a :*: b) = HProduct (from a) (from b)
  to (HProduct a b) = to a :*: to b

  encode = HProduct (encode @(f a)) (encode @(g a))
  decode = HProduct (decode @(f a)) (decode @(g a))


instance Table (f a) => Table (M1 S ('MetaSel ('Just name) x y z) f a) where
  type Schema (M1 S ('MetaSel ('Just name) x y z) f a) =
    Compose (FieldName name) (Schema (f a))

  from = Compose . FieldName . from . unM1
  to = M1 . to . unFieldName . getCompose

  encode = Compose $ FieldName $ encode @(f a)
  decode = Compose $ FieldName $ decode @(f a)


instance Table (f a) => Table (M1 S ('MetaSel 'Nothing x y z) f a) where
  type Schema (M1 S ('MetaSel 'Nothing x y z) f a) =
    Schema (f a)

  from = from . unM1
  to = M1 . to

  encode = coerce (encode @(f a))
  decode = coerce (decode @(f a))


instance Table a => Table (K1 i a x) where
  type Schema (K1 i a x) = Schema a
  from = from . unK1
  to = K1 . to

  encode = encode @a
  decode = decode @a


-- Base types are one column tables.
newtype PostgreSQLSimpleField a =
  PostgreSQLSimpleField a


instance (FromField a, ToField a) => Table (PostgreSQLSimpleField a) where
  type Schema (PostgreSQLSimpleField a) = HIdentity a
  from = coerce
  to = coerce
  decode = coerce $ fromField @a
  encode = coerce $ toField @a


deriving via (PostgreSQLSimpleField Bool) instance Table Bool


deriving via (PostgreSQLSimpleField Double) instance Table Double


deriving via (PostgreSQLSimpleField Float) instance Table Float


deriving via (PostgreSQLSimpleField Int) instance Table Int


deriving via (PostgreSQLSimpleField Int16) instance Table Int16


deriving via (PostgreSQLSimpleField Int32) instance Table Int32


deriving via (PostgreSQLSimpleField Int64) instance Table Int64


deriving via (PostgreSQLSimpleField Integer) instance Table Integer


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

  encode = HIdentity $ showEncoder @a
  decode = HIdentity $ readDecoder @a


-- | This @newtype@ can be used to derive 'Table' instances for types that are
-- stored in the database as a single JSON column, using Aeson's 'ToJSON' and
-- 'FromJSON' type classes as a serialization format.
newtype JSONColumn a = JSONColumn a


instance (FromJSON a, ToJSON a, Typeable a) => Table (JSONColumn a) where
  type Schema (JSONColumn a) = HIdentity a
  to = coerce
  from = coerce

  encode = HIdentity $ jsonEncoder @a
  decode = HIdentity $ jsonDecoder @a


newtype CompositeColumn a = CompositeColumn a


-- TODO
instance (ADT a, Constraints a Table, FromField a) => Table (CompositeColumn a) where
  type Schema (CompositeColumn a) = HIdentity a
  to = coerce
  from = coerce

  encode = undefined

  -- TODO We will have to write some kind of parser here. postgresql-simple doesn't
  -- have good support for composite types. For now, force the user to do this.
  decode = coerce $ fromField @a


rowParser :: Table a => RowParser a
rowParser = rowParser'
  where
    rowParser' :: forall a. Table a => RowParser a
    rowParser' = to <$> htraverse (fmap Identity . columnDecoderToRowParser) (decode @a)


instance Table a => Table (Maybe a) where
  type Schema (Maybe a) =
    HProduct (HIdentity Bool) (HCompose (Schema a) Maybe)

  from =
    maybe
      (HProduct (coerce True) $ htabulate \(I _) -> Identity Nothing)
      (\x -> HProduct (coerce False) $ htabulate \(I i) -> Just <$> hindex (from x) i)

  to (HProduct _ (HCompose x)) = do
    to <$> htraverse (\(Compose y) -> Identity <$> runIdentity y) x

  encode =
    HProduct (encode @Bool) $ HCompose $ hmap (Compose . nullEncoder) $ encode @a

  decode =
    HProduct (decode @Bool) $ HCompose $ hmap (Compose . maybeColumnDecoder) $ decode @a
