{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Label
  ( HLabel, Label
  , hlabel, hunlabel
  )
where

-- base
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Prelude

-- rel8
import Rel8.Kind.Labels ( SLabels( SLabels ) )
import Rel8.Schema.HTable
  ( HTable
  , hfield, htabulate, hspecs
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.FCF
import Rel8.Schema.HTable.MapTable
import GHC.Generics (Generic)


type HLabel :: Symbol -> K.HTable -> K.HTable
newtype HLabel label table context = HLabel (HMapTable (Label label) table context)
  deriving stock Generic
  deriving anyclass HTable


data Label :: Symbol -> Spec -> Exp Spec


type instance Eval (Label label ('Spec labels necessity a)) = 'Spec (label : labels) necessity a


instance KnownSymbol l => MapSpec (Label l) where
  mapInfo = \case
    SSpec {..} -> SSpec {labels = SLabels Proxy labels, ..}


hlabel :: (HTable t, KnownSymbol label)
  => (forall labels necessity a. ()
    => context ('Spec labels necessity a)
    -> context ('Spec (label ': labels) necessity a))
  -> t context
  -> HLabel label t context
hlabel labeler a = HLabel $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    SSpec {} -> labeler (hfield a field)
{-# INLINABLE hlabel #-}


hunlabel :: (HTable t, KnownSymbol label)
  => (forall labels necessity a. ()
    => context ('Spec (label ': labels) necessity a)
    -> context ('Spec labels necessity a))
  -> HLabel label t context
  -> t context
hunlabel unlabler (HLabel as) =
  htabulate $ \field -> 
    case hfield hspecs field of
      SSpec {} -> case hfield as (HMapTableField field) of
        a -> unlabler a
{-# INLINABLE hunlabel #-}
