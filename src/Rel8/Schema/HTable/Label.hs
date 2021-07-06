{-# language DataKinds #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Schema.HTable.Label
  ( HLabel, hlabel, hrelabel, hunlabel
  )
where

-- base
import Data.Kind ( Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude

-- rel8
import Rel8.Schema.HTable
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec(..) )


type HLabel :: Symbol -> K.HTable -> K.HTable
newtype HLabel label table context = HLabel (table context)


type HLabelField :: Symbol -> K.HTable -> Type -> Type
newtype HLabelField label table a = HLabelField (HField table a)


instance (HTable table, KnownSymbol label) => HTable (HLabel label table) where
  type HField (HLabel label table) = HLabelField label table
  type HConstrainTable (HLabel label table) constraint =
    HConstrainTable table constraint

  hfield (HLabel a) (HLabelField field) = hfield a field
  htabulate f = HLabel (htabulate (f . HLabelField))
  htraverse f (HLabel a) = HLabel <$> htraverse f a
  hdicts = HLabel (hdicts @table)
  hspecs = HLabel $ htabulate $ \field -> case hfield (hspecs @table) field of
    Spec {..} -> Spec {labels = symbolVal (Proxy @label) : labels, ..}
  {-# INLINABLE hspecs #-}


hlabel :: forall label t context. t context -> HLabel label t context
hlabel = HLabel
{-# INLINABLE hlabel #-}


hrelabel :: forall label' label t context. HLabel label t context -> HLabel label' t context
hrelabel = hlabel . hunlabel
{-# INLINABLE hrelabel #-}


hunlabel :: forall label t context. HLabel label t context -> t context
hunlabel (HLabel a) = a
{-# INLINABLE hunlabel #-}
