{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.HTable.HMaybeTable ( HMaybeTable(..) ) where

-- base
import GHC.Generics ( Generic )

-- rel8
import Rel8.HTable ( HField, HTable, hdbtype, hfield, htabulate, htraverse )
import Rel8.HTable.Identity ( HIdentity( unHIdentity, HIdentity ) )


data HMaybeTable g f = HMaybeTable
  { hnullTag :: HIdentity (Maybe Bool) f
  , htable :: g f
  }
  deriving stock Generic


data HMaybeField g a where
  HNullTag :: HMaybeField g (Maybe Bool)
  HMaybeField :: HField g a -> HMaybeField g a


instance HTable g => HTable (HMaybeTable g) where
  type HField (HMaybeTable g) = HMaybeField g

  hfield HMaybeTable{ hnullTag, htable } = \case
    HNullTag      -> unHIdentity hnullTag
    HMaybeField i -> hfield htable i

  htabulate f = HMaybeTable (HIdentity (f HNullTag)) (htabulate (f . HMaybeField))

  htraverse f HMaybeTable{ hnullTag, htable } =
    HMaybeTable <$> htraverse f hnullTag <*> htraverse f htable

  hdbtype = HMaybeTable hdbtype hdbtype
