{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Schema.HTable.Identity (
  HIdentity (HIdentity, unHIdentity),
)
where

-- base
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Prelude

-- rel8
import Rel8.Schema.Dict (Dict (Dict))
import Rel8.Schema.HTable (
  HConstrainTable,
  HField,
  HTable,
  hdicts,
  hfield,
  hspecs,
  htabulate,
  htraverse,
 )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null (Sql)
import Rel8.Schema.Spec (specification)
import Rel8.Type (DBType)


type HIdentity :: Type -> K.HTable
newtype HIdentity a context = HIdentity
  { unHIdentity :: context a
  }


instance Sql DBType a => HTable (HIdentity a) where
  type HConstrainTable (HIdentity a) constraint = constraint a
  type HField (HIdentity a) = (:~:) a


  hfield (HIdentity a) Refl = a
  htabulate f = HIdentity $ f Refl
  htraverse f (HIdentity a) = HIdentity <$> f a
  hdicts = HIdentity Dict
  hspecs = HIdentity specification
