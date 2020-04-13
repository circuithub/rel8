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

import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.ByteString ( ByteString )
import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Contravariant ( Op(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor ( HFunctor(..) )
import Data.Indexed.Functor.Compose ( HCompose(..), I(..) )
import Data.Indexed.Functor.Constrained ( HConstrained(..) )
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
class (HConstrained (Pattern a), HTraversable (Pattern a), HRepresentable (Pattern a)) => Table (a :: Type) where
  -- | A higher-kinded pattern functor for this table.
  --
  -- This is a bit like a generic encoding of 'a', but lifted to higher-kinded
  -- data.
  --
  -- This is an injective type family rather than a data family to aid
  -- generic deriving.
  type Pattern a = (r :: (Type -> Type) -> Type) | r -> a
  type Pattern a = Compose (Tagged a) (GPattern (Rep a))

  from :: a -> Pattern a Identity
  default from
    :: (Generic a, GTable (Rep a), Compose (Tagged a) (GPattern (Rep a)) ~ Pattern a)
    => a -> Pattern a Identity
  from = Compose . Tagged . gfrom . GHC.Generics.from

  to :: Pattern a Identity -> a
  default to
    :: (Generic a, GTable (Rep a), Compose (Tagged a) (GPattern (Rep a)) ~ Pattern a)
    => Pattern a Identity -> a
  to = GHC.Generics.to . gto . unTagged . getCompose


  decode :: Pattern a (ReaderT Field (ReaderT (Maybe ByteString) Conversion))
  default decode
    :: (GTable (Rep a), Compose (Tagged a) (GPattern (Rep a)) ~ Pattern a)
    => Pattern a (ReaderT Field (ReaderT (Maybe ByteString) Conversion))
  decode = Compose $ Tagged $ gdecode (Proxy @(Rep a ()))

  encode :: Pattern a (Op O.Literal)
  default encode
    :: (GTable (Rep a), Compose (Tagged a) (GPattern (Rep a)) ~ Pattern a)
    => Pattern a (Op O.Literal)
  encode = Compose $ Tagged $ gencode (Proxy @(Rep a ()))


class GTable (f :: * -> *) where
  type GPattern f :: (* -> *) -> *

  gfrom :: f x -> GPattern f Identity
  gto :: GPattern f Identity -> f x

  gdecode :: Proxy (f x) -> GPattern f (ReaderT Field (ReaderT (Maybe ByteString) Conversion))
  gencode :: Proxy (f x) -> GPattern f (Op O.Literal)


instance GTable f => GTable (M1 D c f) where
  type GPattern (M1 D c f) = GPattern f
  gfrom = gfrom . unM1
  gto = M1 . gto

  gencode proxy = gencode (unM1 <$> proxy)
  gdecode proxy = gdecode (unM1 <$> proxy)


instance GTable f => GTable (M1 C c f) where
  type GPattern (M1 C c f) = GPattern f
  gfrom = gfrom . unM1
  gto = M1 . gto

  gencode proxy = gencode (unM1 <$> proxy)
  gdecode proxy = gdecode (unM1 <$> proxy)


instance (GTable f, GTable g) => GTable (f :*: g) where
  type GPattern (f :*: g) = HProduct (GPattern f) (GPattern g)

  gfrom (a :*: b) = HProduct (gfrom a) (gfrom b)
  gto (HProduct a b) = gto a :*: gto b

  gencode proxy = HProduct (gencode (fmap (\(x :*: _) -> x) proxy)) (gencode (fmap (\(_ :*: y) -> y) proxy))
  gdecode proxy = HProduct (gdecode (fmap (\(x :*: _) -> x) proxy)) (gdecode (fmap (\(_ :*: y) -> y) proxy))


instance GTable f => GTable (M1 S ('MetaSel ('Just name) x y z) f) where
  type GPattern (M1 S ('MetaSel ('Just name) x y z) f) =
    Compose (Tagged name) (GPattern f)

  gfrom = Compose . Tagged . gfrom . unM1
  gto = M1 . gto . unTagged . getCompose

  gencode proxy = Compose $ Tagged $ gencode (unM1 <$> proxy)
  gdecode proxy = Compose $ Tagged $ gdecode (unM1 <$> proxy)


instance GTable f => GTable (M1 S ('MetaSel 'Nothing x y z) f) where
  type GPattern (M1 S ('MetaSel 'Nothing x y z) f) =
    GPattern f

  gfrom = gfrom . unM1
  gto = M1 . gto

  gencode proxy = gencode (unM1 <$> proxy)
  gdecode proxy = gdecode (unM1 <$> proxy)


instance Table a => GTable (K1 i a) where
  type GPattern (K1 i a) = Pattern a
  gfrom = from . unK1
  gto = K1 . to

  gencode _ = encode
  gdecode _ = decode



-- Base types are one column tables.


instance Table Bool where
  type Pattern Bool = HIdentity Bool
  from = coerce
  to = coerce
  decode = HIdentity $ ReaderT \field -> ReaderT \mByteString -> fromField field mByteString
  encode = HIdentity $ Op O.BoolLit


instance Table Int where
  type Pattern Int = HIdentity Int
  from = coerce
  to = coerce
  decode = HIdentity $ ReaderT \field -> ReaderT \mByteString -> fromField field mByteString
  encode = HIdentity $ Op $ O.IntegerLit . fromIntegral


instance (Table a, Table b) => Table (a, b) where
  decode = Compose $ Tagged $ HProduct decode decode
  encode = Compose $ Tagged $ HProduct encode encode


rowParser :: Table a => RowParser a
rowParser = fmap to $ htraverse (coerce fieldWith) decode


instance Table a => Table (Maybe a) where
  type Pattern (Maybe a) =
    Compose (Tagged (Maybe a)) (HProduct (HIdentity Bool) (HCompose (Pattern a) Maybe))

  from =
    maybe
      (Compose $ Tagged $ HProduct (HIdentity $ pure True) $ htabulate \(I _) -> Identity Nothing)
      (\x -> Compose $ Tagged $ HProduct (HIdentity $ pure False) $ htabulate \(I i) -> Just <$> hindex (from x) i)

  to (Compose (Tagged (HProduct _ (HCompose x)))) = do
    to <$> htraverse (\(Compose y) -> Identity <$> runIdentity y) x

  encode =
    Compose $ Tagged $ HProduct encode $ HCompose $ hmap (\(Op f) -> Compose $ Op $ maybe O.NullLit f) $ encode @a

  decode = Compose $ Tagged $ HProduct decode $ HCompose $ hmap (\f -> Compose (nullIsNothing f)) $ decode @a
    where
      nullIsNothing parser = ReaderT \field -> ReaderT (maybe (pure Nothing) (runReaderT (runReaderT (Just <$> parser) field) . Just))
