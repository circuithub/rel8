{-# language DataKinds #-}
{-# language GADTs #-}
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
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.Spec ( Context, Spec, KnownSpec, specSing )


type HIdentity :: Spec -> HKTable
data HIdentity spec context where
  HIdentity ::
    { unHIdentity :: context spec
    } -> HIdentity spec context


type HIdentityField :: Spec -> Context
data HIdentityField _spec spec where
  HIdentityField :: HIdentityField spec spec


instance KnownSpec spec => HTable (HIdentity spec) where
  type HConstrainTable (HIdentity spec) c = c spec
  type HField (HIdentity spec) = HIdentityField spec

  hfield (HIdentity a) HIdentityField = a
  htabulate f = HIdentity $ f HIdentityField
  htraverse f (HIdentity a) = HIdentity <$> f a
  hdicts = HIdentity Dict
  hspecs = HIdentity specSing
