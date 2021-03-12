{-# language DataKinds #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.HTable.Identity
  ( HIdentity(..)
  )
where

-- base
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.Spec ( Context, Spec, KnownSpec( specSing ) )


{-| A single-column higher-kinded table. This is primarily useful for
facilitating generic-deriving of higher kinded tables.
-}
type HIdentity :: Spec -> HKTable
data HIdentity spec context where
  HIdentity ::
    { unHIdentity :: context spec
    } -> HIdentity spec (H context)


type HIdentityField :: Spec -> Context
data HIdentityField _spec spec where
  HIdentityField :: HIdentityField spec spec


instance KnownSpec spec => HTable (HIdentity spec) where
  type HConstrainTable (HIdentity spec) c = c spec
  type HField (HIdentity spec) = HIdentityField spec

  hfield (HIdentity a) HIdentityField = a
  htabulate f = HIdentity $ f HIdentityField
  hdicts = HIdentity Dict
  hspecs = HIdentity specSing

  htraverse :: forall f g m. Applicative m => (forall x. f x -> m (g x)) -> HIdentity spec (H f) -> m (HIdentity spec (H g))
  htraverse f (HIdentity a) = HIdentity <$> f (a :: f spec)
