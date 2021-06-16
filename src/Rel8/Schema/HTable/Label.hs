{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
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
import Rel8.Kind.Labels ( SLabels( SCons ) )
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


type instance Eval (Label label ('Spec labels defaulting a)) = 'Spec (label : labels) defaulting a


instance KnownSymbol l => MapSpec (Label l) where
  mapInfo = \case
    SSpec {..} -> SSpec {labels = SCons Proxy labels, ..}


hlabel :: (HTable t, KnownSymbol label)
  => (forall labels defaulting a. ()
    => context ('Spec labels defaulting a)
    -> context ('Spec (label ': labels) defaulting a))
  -> t context
  -> HLabel label t context
hlabel labeler a = HLabel $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    SSpec {} -> labeler (hfield a field)
{-# INLINABLE hlabel #-}


hunlabel :: (HTable t, KnownSymbol label)
  => (forall labels defaulting a. ()
    => context ('Spec (label ': labels) defaulting a)
    -> context ('Spec labels defaulting a))
  -> HLabel label t context
  -> t context
hunlabel unlabler (HLabel as) =
  htabulate $ \field -> 
    case hfield hspecs field of
      SSpec {} -> case hfield as (HMapTableField field) of
        a -> unlabler a
{-# INLINABLE hunlabel #-}
