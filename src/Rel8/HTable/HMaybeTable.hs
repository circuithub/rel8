{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.HTable.HMaybeTable ( HMaybeTable(..), MakeNull ) where

-- base
import Data.Kind ( Type )
import GHC.Generics ( Generic )

-- rel8
import Rel8.HTable ( HField, HTable, hdbtype, hfield, htabulate, htraverse )
import Rel8.HTable.HMapTable ( Eval, Exp, HMapTable, MapInfo( mapInfo ) )
import Rel8.HTable.Identity ( HIdentity( unHIdentity, HIdentity ) )
import Rel8.Info ( Info( Null, NotNull ), Nullify )


data MakeNull :: Type -> Exp Type


type instance Eval (MakeNull x) = Nullify x


instance MapInfo MakeNull where
  mapInfo = \case
    NotNull t -> Null t
    Null t    -> Null t


data HMaybeTable g f = HMaybeTable
  { hnullTag :: HIdentity (Maybe Bool) f
  , htable :: HMapTable MakeNull g f
  }
  deriving stock Generic


data HMaybeField g a where
  HNullTag :: HMaybeField g (Maybe Bool)
  HMaybeField :: HField (HMapTable MakeNull g) a -> HMaybeField g a


instance HTable g => HTable (HMaybeTable g) where
  type HField (HMaybeTable g) = HMaybeField g

  hfield HMaybeTable{ hnullTag, htable } = \case
    HNullTag      -> unHIdentity hnullTag
    HMaybeField i -> hfield htable i

  htabulate f = HMaybeTable (HIdentity (f HNullTag)) (htabulate (f . HMaybeField))

  htraverse f HMaybeTable{ hnullTag, htable } =
    HMaybeTable <$> htraverse f hnullTag <*> htraverse f htable

  hdbtype = HMaybeTable hdbtype hdbtype
