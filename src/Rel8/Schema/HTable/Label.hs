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
  ( HLabel( HLabel )
  , hlabel, hunlabel
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Prelude

-- rel8
import Rel8.Kind.Labels ( SLabels( SLabels ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )


type HLabel :: Symbol -> K.HTable -> K.HTable
data HLabel label table context where
  HLabel :: table (LabelSpec label context) -> HLabel label table context


type HLabelField :: Symbol -> K.HTable -> Spec -> Type
data HLabelField label table spec where
  HLabelField
    :: HField table ('Spec labels necessity db a)
    -> HLabelField label table ('Spec (label ': labels) necessity db a)


instance (HTable table, KnownSymbol label) => HTable (HLabel label table) where
  type HField (HLabel label table) = HLabelField label table
  type HConstrainTable (HLabel label table) c =
    HConstrainTable table (LabelSpecC label c)

  hfield (HLabel table) (HLabelField field) =
    getLabelSpec (hfield table field)

  htabulate f = HLabel $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> LabelSpec (f (HLabelField field))
  htraverse f (HLabel t) = HLabel <$> htraverse (traverseLabelSpec f) t

  hdicts :: forall c. HConstrainTable table (LabelSpecC label c)
    => HLabel label table (Dict c)
  hdicts = HLabel $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield (hdicts @_ @(LabelSpecC label c)) field of
      Dict -> LabelSpec Dict

  hspecs = HLabel $ htabulate $ \field -> case hfield hspecs field of
    SSpec {..} -> LabelSpec SSpec {labels = SLabels Proxy labels, ..}

  {-# INLINABLE hfield #-}
  {-# INLINABLE htabulate #-}
  {-# INLINABLE htraverse #-}
  {-# INLINABLE hdicts #-}
  {-# INLINABLE hspecs #-}


type LabelingSpec :: Type -> Type
type LabelingSpec r = Symbol -> (Spec -> r) -> Spec -> r


type LabelSpec :: LabelingSpec Type
data LabelSpec label context spec where
  LabelSpec
    :: { getLabelSpec :: context ('Spec (label ': labels) necessity db a) }
    -> LabelSpec label context ('Spec labels necessity db a)


type LabelSpecC :: LabelingSpec Constraint
class
  ( forall labels necessity db a.
    ( spec ~ 'Spec labels necessity db a =>
       constraint ('Spec (label ': labels) necessity db a)
    )
  ) => LabelSpecC label constraint spec
instance
  ( spec ~ 'Spec labels necessity db a
  , constraint ('Spec (label ': labels) necessity db a)
  ) => LabelSpecC label constraint spec


traverseLabelSpec :: forall context context' label spec m. Functor m
  => (forall x. context x -> m (context' x))
  -> LabelSpec label context spec -> m (LabelSpec label context' spec)
traverseLabelSpec f (LabelSpec a) = LabelSpec <$> f a


hlabel :: HTable t
  => (forall labels necessity db a. ()
    => context ('Spec labels necessity db a)
    -> context ('Spec (label ': labels) necessity db a))
  -> t context
  -> HLabel label t context
hlabel labeler a = HLabel $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {} -> LabelSpec (labeler (hfield a field))
{-# INLINABLE hlabel #-}


hunlabel :: HTable t
  => (forall labels necessity db a. ()
    => context ('Spec (label ': labels) necessity db a)
    -> context ('Spec labels necessity db a))
  -> HLabel label t context
  -> t context
hunlabel unlabler (HLabel as) =
  htabulate $ \field -> case hfield as field of
      LabelSpec a -> unlabler a
{-# INLINABLE hunlabel #-}
