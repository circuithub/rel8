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
  , hmap
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Functor.Compose ( Compose( Compose ), getCompose )
import Data.Functor.Identity ( runIdentity )
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
import Rel8.Schema.Spec ( Spec, SSpec )
import Rel8.Schema.HTable.Pair ( HPair( HPair ) )
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
  type HField t = (field :: Spec -> Type) | field -> t
  type HConstrainTable t (c :: Spec -> Constraint) :: Constraint

  hfield :: t context -> HField t spec -> context spec
  htabulate :: (forall spec. HField t spec -> context spec) -> t context
  htraverse :: Apply m => (forall spec. f spec -> m (g spec)) -> t f -> m (t g)
  hdicts :: HConstrainTable t c => t (Dict c)
  hspecs :: t SSpec

  type HField t = GHField t
  type HConstrainTable t c = HConstrainTable (GHColumns (Rep (t Proxy))) c

  default hfield ::
    ( Generic (t context)
    , HField t ~ GHField t
    , HField (GHColumns (Rep (t Proxy))) ~ HField (GHColumns (Rep (t context)))
    , GHTable context (Rep (t context))
    )
    => t context -> HField t spec -> context spec
  hfield table (GHField field) = hfield (toGHColumns (from table)) field

  default htabulate ::
    ( Generic (t context)
    , HField t ~ GHField t
    , HField (GHColumns (Rep (t Proxy))) ~ HField (GHColumns (Rep (t context)))
    , GHTable context (Rep (t context))
    )
    => (forall spec. HField t spec -> context spec) -> t context
  htabulate f = to $ fromGHColumns $ htabulate (f . GHField)

  default htraverse
    :: forall f g m
     . ( Apply m
       , Generic (t f), GHTable f (Rep (t f))
       , Generic (t g), GHTable g (Rep (t g))
       , GHColumns (Rep (t f)) ~ GHColumns (Rep (t g))
       )
    => (forall spec. f spec -> m (g spec)) -> t f -> m (t g)
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
    ( Generic (t SSpec)
    , GHTable SSpec (Rep (t SSpec))
    )
    => t SSpec
  hspecs = to $ fromGHColumns hspecs

  {-# INLINABLE hfield #-}
  {-# INLINABLE htabulate #-}
  {-# INLINABLE htraverse #-}
  {-# INLINABLE hdicts #-}
  {-# INLINABLE hspecs #-}


htabulateA :: (HTable t, Apply m)
  => (forall spec. HField t spec -> m (context spec)) -> m (t context)
htabulateA f = htraverse getCompose $ htabulate $ Compose . f
{-# INLINABLE htabulateA #-}


type GHField :: K.HTable -> Spec -> Type
newtype GHField t spec = GHField (HField (GHColumns (Rep (t Proxy))) spec)


type GHTable :: K.HContext -> (Type -> Type) -> Constraint
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
  type GHColumns (a :*: b) = HPair (GHColumns a) (GHColumns b)
  toGHColumns (a :*: b) = HPair (toGHColumns a) (toGHColumns b)
  fromGHColumns (HPair a b) = fromGHColumns a :*: fromGHColumns b


-- | A HField type for indexing into HPair.
type HPairField :: K.HTable -> K.HTable -> Spec -> Type
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


hmap :: HTable t => (forall spec. f spec -> g spec) -> t f -> t g
hmap f = runIdentity . htraverse (pure . f)
