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
  , htabulateA

  , HPair(..)
  )
where

-- base
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

-- rel8
import Rel8.Schema.Dict ( Dict )
import Rel8.Schema.Spec ( Spec, SSpec, Context )
import Rel8.Schema.HTable.Context ( HKTable, H )
import Rel8.Schema.HTable.Pair ( HPair( HPair ) )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )


type HTable :: HKTable -> Constraint
class HTable t where
  type HField t = (field :: Context) | field -> t
  type HConstrainTable t (c :: Spec -> Constraint) :: Constraint

  hfield :: t (H context) -> HField t spec -> context spec
  htabulate :: (forall spec. HField t spec -> context spec) -> t (H context)
  htraverse :: Apply m => (forall spec. f spec -> m (g spec)) -> t (H f) -> m (t (H g))
  hdicts :: HConstrainTable t c => t (H (Dict c))
  hspecs :: t (H SSpec)

  type HField t = GHField t
  type HConstrainTable t c = HConstrainTable (GHColumns (Rep (t (H Proxy)))) c

  default hfield ::
    ( Generic (t (H context))
    , HField t ~ GHField t
    , HField (GHColumns (Rep (t (H Proxy)))) ~ HField (GHColumns (Rep (t (H context))))
    , GHTable context (Rep (t (H context)))
    )
    => t (H context) -> HField t spec -> context spec
  hfield table (GHField field) = hfield (toGHColumns (from table)) field

  default htabulate ::
    ( Generic (t (H context))
    , HField t ~ GHField t
    , HField (GHColumns (Rep (t (H Proxy)))) ~ HField (GHColumns (Rep (t (H context))))
    , GHTable context (Rep (t (H context)))
    )
    => (forall spec. HField t spec -> context spec) -> t (H context)
  htabulate f = to $ fromGHColumns $ htabulate (f . GHField)

  default htraverse
    :: forall f g m
     . ( Apply m
       , Generic (t (H f)), GHTable f (Rep (t (H f)))
       , Generic (t (H g)), GHTable g (Rep (t (H g)))
       , GHColumns (Rep (t (H f))) ~ GHColumns (Rep (t (H g)))
       )
    => (forall spec. f spec -> m (g spec)) -> t (H f) -> m (t (H g))
  htraverse f = fmap (to . fromGHColumns) . htraverse f . toGHColumns . from

  default hdicts
    :: forall c
     . ( Generic (t (H (Dict c)))
       , GHTable (Dict c) (Rep (t (H (Dict c))))
       , GHColumns (Rep (t (H Proxy))) ~ GHColumns (Rep (t (H (Dict c))))
       , HConstrainTable (GHColumns (Rep (t (H Proxy)))) c
       )
    => t (H (Dict c))
  hdicts = to $ fromGHColumns (hdicts @(GHColumns (Rep (t (H Proxy)))) @c)

  default hspecs ::
    ( Generic (t (H SSpec))
    , GHTable SSpec (Rep (t (H SSpec)))
    )
    => t (H SSpec)
  hspecs = to $ fromGHColumns hspecs

  {-# INLINABLE hfield #-}
  {-# INLINABLE htabulate #-}
  {-# INLINABLE htraverse #-}
  {-# INLINABLE hdicts #-}
  {-# INLINABLE hspecs #-}


htabulateA :: (HTable t, Apply m)
  => (forall spec. HField t spec -> m (context spec))
  -> m (t (H context))
htabulateA f = htraverse getCompose $ htabulate $ Compose . f
{-# INLINABLE htabulateA #-}


type GHField :: HKTable -> Context
newtype GHField t spec = GHField (HField (GHColumns (Rep (t (H Proxy)))) spec)


type GHTable :: Context -> (Type -> Type) -> Constraint
class HTable (GHColumns rep) => GHTable context rep | rep -> context where
  type GHColumns rep :: HKTable
  toGHColumns :: rep x -> GHColumns rep (H context)
  fromGHColumns :: GHColumns rep (H context) -> rep x


instance GHTable context rep => GHTable context (M1 i c rep) where
  type GHColumns (M1 i c rep) = GHColumns rep
  toGHColumns (M1 a) = toGHColumns a
  fromGHColumns = M1 . fromGHColumns


instance HTable table => GHTable context (K1 i (table (H context))) where
  type GHColumns (K1 i (table (H context))) = table
  toGHColumns (K1 a) = a
  fromGHColumns = K1


instance (GHTable context a, GHTable context b) => GHTable context (a :*: b) where
  type GHColumns (a :*: b) = HPair (GHColumns a) (GHColumns b)
  toGHColumns (a :*: b) = HPair (toGHColumns a) (toGHColumns b)
  fromGHColumns (HPair a b) = fromGHColumns a :*: fromGHColumns b


-- | A HField type for indexing into HPair.
type HPairField :: HKTable -> HKTable -> Context
data HPairField x y spec
  = HFst (HField x spec)
  | HSnd (HField y spec)


instance (HTable x, HTable y) => HTable (HPair x y) where
  type HConstrainTable (HPair x y) c = (HConstrainTable x c, HConstrainTable y c)
  type HField (HPair x y) = HPairField x y

  hfield (HPair l r) = \case
    HFst i -> hfield l i
    HSnd i -> hfield r i

  htabulate f = HPair (htabulate (f . HFst)) (htabulate (f . HSnd))
  htraverse f (HPair x y) = HPair <$> htraverse f x <.> htraverse f y
  hdicts = HPair hdicts hdicts
  hspecs = HPair hspecs hspecs

  {-# INLINABLE hfield #-}
  {-# INLINABLE htabulate #-}
  {-# INLINABLE htraverse #-}
  {-# INLINABLE hdicts #-}
  {-# INLINABLE hspecs #-}
