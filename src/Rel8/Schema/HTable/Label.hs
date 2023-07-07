{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Schema.HTable.Label (
  HLabel,
  hlabel,
  hrelabel,
  hunlabel,
  hproject,
)
where

-- base
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude

-- rel8
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
import Rel8.Schema.Spec (Spec (..))


type HLabel :: Symbol -> K.HTable -> K.HTable
newtype HLabel label table context = HLabel (table context)


type HLabelField :: Symbol -> K.HTable -> Type -> Type
newtype HLabelField label table a = HLabelField (HField table a)


instance (HTable table, KnownSymbol label) => HTable (HLabel label table) where
  type HField (HLabel label table) = HLabelField label table
  type
    HConstrainTable (HLabel label table) constraint =
      HConstrainTable table constraint


  hfield (HLabel a) (HLabelField field) = hfield a field
  htabulate f = HLabel (htabulate (f . HLabelField))
  htraverse f (HLabel a) = HLabel <$> htraverse f a
  hdicts = HLabel (hdicts @table)
  hspecs = HLabel $ htabulate $ \field -> case hfield (hspecs @table) field of
    Spec{..} -> Spec{labels = symbolVal (Proxy @label) : labels, ..}
  {-# INLINEABLE hspecs #-}


hlabel :: forall label t context. t context -> HLabel label t context
hlabel = HLabel
{-# INLINEABLE hlabel #-}


hrelabel :: forall label' label t context. HLabel label t context -> HLabel label' t context
hrelabel = hlabel . hunlabel
{-# INLINEABLE hrelabel #-}


hunlabel :: forall label t context. HLabel label t context -> t context
hunlabel (HLabel a) = a
{-# INLINEABLE hunlabel #-}


hproject ::
  () =>
  (forall ctx. t ctx -> t' ctx) ->
  HLabel label t context ->
  HLabel label t' context
hproject f (HLabel a) = HLabel (f a)
