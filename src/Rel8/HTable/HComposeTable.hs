{-# language DataKinds #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module Rel8.HTable.HComposeTable
  ( HComposeTable(..)
  , ComposeInner(..)
  , zipComposeInnerWith
  ) where

-- rel8
import Rel8.Context ( Context, KContext )
import Rel8.DBFunctor ( DBFunctor( liftDatabaseType ) )
import Rel8.DatabaseType ( DatabaseType )
import Rel8.HTable ( HTable( HField, hfield, htabulate, htraverse, hdbtype ), hmap )


newtype HComposeTable g t (f :: KContext) = HComposeTable (t (Context (ComposeInner f g)))


data HComposeField f t a where
  HComposeField :: HField t a -> HComposeField f t (f a)


instance (HTable t, DBFunctor f) => HTable (HComposeTable f t) where
  type HField (HComposeTable f t) = HComposeField f t

  hfield (HComposeTable columns) (HComposeField field) =
    getComposeInner (hfield columns field)

  htabulate f = HComposeTable (htabulate (ComposeInner . f . HComposeField))

  htraverse f (HComposeTable t) = HComposeTable <$> htraverse (traverseComposeInner f) t

  hdbtype :: HComposeTable f t (Context DatabaseType)
  hdbtype = HComposeTable $ hmap (ComposeInner . liftDatabaseType) hdbtype


data ComposeInner context g a where
  ComposeInner :: { getComposeInner :: f (g a) } -> ComposeInner (Context f) g a


traverseComposeInner :: forall f g t m a. Applicative m
  => (forall x. f x -> m (g x))
  -> ComposeInner (Context f) t a -> m (ComposeInner (Context g) t a)
traverseComposeInner f (ComposeInner a) =
  ComposeInner <$> f a


zipComposeInnerWith :: forall f g h t a. ()
  => (forall x. f x -> g x -> h x)
  -> ComposeInner (Context f) t a -> ComposeInner (Context g) t a -> ComposeInner (Context h) t a
zipComposeInnerWith f (ComposeInner a) (ComposeInner b) =
  ComposeInner $ f a b
