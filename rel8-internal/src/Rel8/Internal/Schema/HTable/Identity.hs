{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Internal.Schema.HTable.Identity
  ( HIdentity( HIdentity, unHIdentity )
  )
where

-- base
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Internal.Schema.Dict ( Dict( Dict ) )
import Rel8.Internal.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Null ( Sql )
import Rel8.Internal.Schema.Spec ( specification )
import Rel8.Internal.Type ( DBType )


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
