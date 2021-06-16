{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.HTable.Identity
  ( HIdentity( HIdentity, HType, unHIdentity )
  , HType
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Kind.Defaulting ( Defaulting( NoDefault ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ), KnownSpec, specSing )


type HType :: Type -> K.HTable
type HType a = HIdentity ('Spec '[] 'NoDefault a)


pattern HType :: context ('Spec '[] 'NoDefault a) -> HType a context
pattern HType a = HIdentity a
{-# COMPLETE HType #-}


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
