{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable
  ( HTable (HField, HConstrainTable)
  , hfield, htabulate, htraverse, hdicts, hspecs
  , hfoldMap, hmap, htabulateA, htraverseP, htraversePWithField
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import Data.Functor.Compose ( Compose( Compose ), getCompose )
import Data.Proxy ( Proxy )
import GHC.Generics
  ( (:*:)( (:*:) )
  , Generic (Rep, from, to)
  , K1( K1 )
  , M1( M1 )
  )
import Prelude

-- profunctors
import Data.Profunctor ( rmap, Profunctor (lmap) )

-- product-profunctors
import Data.Profunctor.Product ( ProductProfunctor ((****)) )

-- rel8
import Rel8.Schema.Dict ( Dict )
import Rel8.Schema.Spec ( Spec )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import qualified Rel8.Schema.Kind as K

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )

-- | A @HTable@ is a functor-indexed/higher-kinded data type that is
-- representable ('htabulate'/'hfield'), constrainable ('hdicts'), and
-- specified ('hspecs').
--
-- This is an internal concept for Rel8, and you should not need to define
-- instances yourself or specify this constraint.
type HTable :: K.HTable -> Constraint
class HTable t where
  type HField t = (field :: Type -> Type) | field -> t
  type HConstrainTable t (c :: Type -> Constraint) :: Constraint

  hfield :: t context -> HField t a -> context a
  htabulate :: (forall a. HField t a -> context a) -> t context
  htraverse :: Apply m => (forall a. f a -> m (g a)) -> t f -> m (t g)
  hdicts :: HConstrainTable t c => t (Dict c)
  hspecs :: t Spec

  type HField t = GHField t
  type HConstrainTable t c = HConstrainTable (GHColumns (Rep (t Proxy))) c

  default hfield ::
    ( Generic (t context)
    , HField t ~ GHField t
    , HField (GHColumns (Rep (t Proxy))) ~ HField (GHColumns (Rep (t context)))
    , GHTable context (Rep (t context))
    )
    => t context -> HField t a -> context a
  hfield table (GHField field) = hfield (toGHColumns (from table)) field

  default htabulate ::
    ( Generic (t context)
    , HField t ~ GHField t
    , HField (GHColumns (Rep (t Proxy))) ~ HField (GHColumns (Rep (t context)))
    , GHTable context (Rep (t context))
    )
    => (forall a. HField t a -> context a) -> t context
  htabulate f = to $ fromGHColumns $ htabulate (f . GHField)

  default htraverse
    :: forall f g m
     . ( Apply m
       , Generic (t f), GHTable f (Rep (t f))
       , Generic (t g), GHTable g (Rep (t g))
       , GHColumns (Rep (t f)) ~ GHColumns (Rep (t g))
       )
    => (forall a. f a -> m (g a)) -> t f -> m (t g)
  htraverse f = fmap (to . fromGHColumns) . htraverse f . toGHColumns . from

  default hdicts
    :: forall c
     . ( Generic (t (Dict c))
       , GHTable (Dict c) (Rep (t (Dict c)))
       , GHColumns (Rep (t Proxy)) ~ GHColumns (Rep (t (Dict c)))
       , HConstrainTable (GHColumns (Rep (t Proxy))) c
       )
    => t (Dict c)
  hdicts = to $ fromGHColumns (hdicts @(GHColumns (Rep (t Proxy))) @c)

  default hspecs ::
    ( Generic (t Spec)
    , GHTable Spec (Rep (t Spec))
    )
    => t Spec
  hspecs = to $ fromGHColumns hspecs

  {-# INLINABLE hfield #-}
  {-# INLINABLE htabulate #-}
  {-# INLINABLE htraverse #-}
  {-# INLINABLE hdicts #-}
  {-# INLINABLE hspecs #-}


hfoldMap :: (HTable t, Semigroup s)
  => (forall a. context a -> s) -> t context -> s
hfoldMap f a = getConst $ htraverse (Const . f) a


hmap :: HTable t
  => (forall a. context a -> context' a) -> t context -> t context'
hmap f a = htabulate $ \field -> f (hfield a field)


htabulateA :: (HTable t, Apply m)
  => (forall a. HField t a -> m (context a)) -> m (t context)
htabulateA f = htraverse getCompose $ htabulate $ Compose . f
{-# INLINABLE htabulateA #-}


newtype ApplyP p a b = ApplyP { unApplyP :: p a b }


instance Profunctor p => Functor (ApplyP p a) where
  fmap f = ApplyP . rmap f . unApplyP


instance ProductProfunctor p => Apply (ApplyP p a) where
  ApplyP f <.> ApplyP x = ApplyP (rmap id f **** x)


htraverseP :: (HTable t, ProductProfunctor p)
  => (forall a. p (f a) (g a)) -> p (t f) (t g)
htraverseP f = htraversePWithField (const f)


htraversePWithField :: (HTable t, ProductProfunctor p)
  => (forall a. HField t a -> p (f a) (g a)) -> p (t f) (t g)
htraversePWithField f = unApplyP $ htabulateA $ \field -> ApplyP $
  lmap (flip hfield field) (f field)


type GHField :: K.HTable -> Type -> Type
newtype GHField t a = GHField (HField (GHColumns (Rep (t Proxy))) a)


type GHTable :: K.Context -> (Type -> Type) -> Constraint
class HTable (GHColumns rep) => GHTable context rep | rep -> context where
  type GHColumns rep :: K.HTable
  toGHColumns :: rep x -> GHColumns rep context
  fromGHColumns :: GHColumns rep context -> rep x


instance GHTable context rep => GHTable context (M1 i c rep) where
  type GHColumns (M1 i c rep) = GHColumns rep
  toGHColumns (M1 a) = toGHColumns a
  fromGHColumns = M1 . fromGHColumns


instance HTable table => GHTable context (K1 i (table context)) where
  type GHColumns (K1 i (table context)) = table
  toGHColumns (K1 a) = a
  fromGHColumns = K1


instance (GHTable context a, GHTable context b) => GHTable context (a :*: b) where
  type GHColumns (a :*: b) = HProduct (GHColumns a) (GHColumns b)
  toGHColumns (a :*: b) = HProduct (toGHColumns a) (toGHColumns b)
  fromGHColumns (HProduct a b) = fromGHColumns a :*: fromGHColumns b


-- | A HField type for indexing into HProduct.
type HProductField :: K.HTable -> K.HTable -> Type -> Type
data HProductField x y a
  = HFst (HField x a)
  | HSnd (HField y a)


instance (HTable x, HTable y) => HTable (HProduct x y) where
  type HConstrainTable (HProduct x y) c = (HConstrainTable x c, HConstrainTable y c)
  type HField (HProduct x y) = HProductField x y

  hfield (HProduct l r) = \case
    HFst i -> hfield l i
    HSnd i -> hfield r i

  htabulate f = HProduct (htabulate (f . HFst)) (htabulate (f . HSnd))
  htraverse f (HProduct x y) = HProduct <$> htraverse f x <.> htraverse f y
  hdicts = HProduct hdicts hdicts
  hspecs = HProduct hspecs hspecs

  {-# INLINABLE hfield #-}
  {-# INLINABLE htabulate #-}
  {-# INLINABLE htraverse #-}
  {-# INLINABLE hdicts #-}
  {-# INLINABLE hspecs #-}
