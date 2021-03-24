{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.HTable.Identity
  ( HIdentity(..)
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec, KnownSpec, specSing )


type HIdentity :: Spec -> K.HTable
newtype HIdentity spec context = HIdentity
  { unHIdentity :: context spec
  }


type HIdentityField :: Spec -> Spec -> Type
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
