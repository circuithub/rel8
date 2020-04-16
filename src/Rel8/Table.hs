{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table where

import Data.Functor.FieldName ( FieldName(..) )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.ByteString ( ByteString )
import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Contravariant ( Op(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Compose ( HCompose(..), I(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Tagged.PolyKinded ( Tagged(..) )
import Database.PostgreSQL.Simple.FromField ( Conversion, Field, fromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import qualified GHC.Generics
import GHC.Generics ( Generic, Rep, M1(..), D, S, C, (:*:)(..), Meta(..), K1(..) )
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
  type Schema a = (r :: (Type -> Type) -> Type) | r -> a
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

  encode :: Schema a (Op O.Literal)
  default encode
    :: (GTable (Rep a), Compose (Tagged a) (GSchema (Rep a)) ~ Schema a)
    => Schema a (Op O.Literal)
  encode = Compose $ Tagged $ gencode (Proxy @(Rep a ()))


class GTable (f :: * -> *) where
  type GSchema f :: (* -> *) -> *

  gfrom :: f x -> GSchema f Identity
  gto :: GSchema f Identity -> f x

  gdecode :: Proxy (f x) -> GSchema f (ReaderT Field (ReaderT (Maybe ByteString) Conversion))
  gencode :: Proxy (f x) -> GSchema f (Op O.Literal)


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

  gencode _ = encode
  gdecode _ = decode



-- Base types are one column tables.


instance Table Bool where
  type Schema Bool = HIdentity Bool
  from = coerce
  to = coerce
  decode = coerce $ fromField @Bool
  encode = coerce O.BoolLit


instance Table Int where
  type Schema Int = HIdentity Int
  from = coerce
  to = coerce
  decode = coerce $ fromField @Int
  encode = coerce (O.IntegerLit . fromIntegral @Int)


instance Table String where
  type Schema String = HIdentity String
  from = coerce
  to = coerce
  decode = coerce $ fromField @String
  encode = coerce O.StringLit


instance (Table a, Table b) => Table (a, b) where
  decode = Compose $ Tagged $ HProduct decode decode
  encode = Compose $ Tagged $ HProduct encode encode


rowParser :: Table a => RowParser a
rowParser = to <$> htraverse (coerce fieldWith) decode


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
    Compose $ Tagged $ HProduct encode $ HCompose $ hmap (\(Op f) -> Compose $ Op $ maybe O.NullLit f) $ encode @a

  decode = Compose $ Tagged $ HProduct decode $ HCompose $ hmap (\f -> Compose (nullIsNothing f)) $ decode @a
    where
      nullIsNothing parser = ReaderT \field -> ReaderT (maybe (pure Nothing) (runReaderT (runReaderT (Just <$> parser) field) . Just))
