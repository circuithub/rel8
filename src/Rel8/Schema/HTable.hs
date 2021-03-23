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
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict )
import Rel8.Schema.Spec ( Spec, SSpec, Context )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Pair ( HPair( HPair ) )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )


type HTable :: ((Spec -> Type) -> Type) -> Constraint
class HTable t where
  type HField t = (field :: Spec -> Type) | field -> t
  type HConstrainTable t (c :: Spec -> Constraint) :: Constraint

  hfield :: t context -> HField t spec -> context spec
  htabulate :: (forall spec. HField t spec -> context spec) -> t context
  htraverse :: Apply m => (forall spec. f spec -> m (g spec)) -> t f -> m (t g)
  hdicts :: HConstrainTable t c => t (Dict c)
  hspecs :: t SSpec


htabulateA :: (HTable t, Apply m)
  => (forall spec. HField t spec -> m (context spec))
  -> m (t context)
htabulateA f = htraverse getCompose $ htabulate $ Compose . f
{-# INLINABLE htabulateA #-}


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
