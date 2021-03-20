{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
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
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec( SSpec ) )


type HLabel :: Symbol -> HKTable -> HKTable
data HLabel label table context where
  HLabel :: table (H (LabelSpec label context)) -> HLabel label table (H context)


type HLabelField :: Symbol -> HKTable -> Context
data HLabelField label table spec where
  HLabelField
    :: HField table ('Spec labels necessity nullability blueprint)
    -> HLabelField label table ('Spec (label ': labels) necessity nullability blueprint)


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
    => HLabel label table (H (Dict c))
  hdicts = HLabel $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield (hdicts @_ @(LabelSpecC label c)) field of
      Dict -> LabelSpec Dict

  hspecs = HLabel $ htabulate $ \field -> case hfield hspecs field of
    SSpec labels necessity nullability blueprint ->
      LabelSpec (SSpec (SLabels Proxy labels) necessity nullability blueprint)

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
    :: { getLabelSpec :: context ('Spec (label ': labels) necessity nullability blueprint) }
    -> LabelSpec label context ('Spec labels necessity nullability blueprint)


type LabelSpecC :: LabelingSpec Constraint
class
  ( forall labels necessity nullability blueprint.
    ( spec ~ 'Spec labels necessity nullability blueprint =>
       constraint ('Spec (label ': labels) necessity nullability blueprint)
    )
  ) => LabelSpecC label constraint spec
instance
  ( spec ~ 'Spec labels necessity nullability blueprint
  , constraint ('Spec (label ': labels) necessity nullability blueprint)
  ) => LabelSpecC label constraint spec


traverseLabelSpec :: forall context context' label spec m. Functor m
  => (forall x. context x -> m (context' x))
  -> LabelSpec label context spec -> m (LabelSpec label context' spec)
traverseLabelSpec f (LabelSpec a) = LabelSpec <$> f a


hlabel :: HTable t
  => (forall labels necessity nullability blueprint. ()
    => context ('Spec labels necessity nullability blueprint)
    -> context ('Spec (label ': labels) necessity nullability blueprint))
  -> t (H context)
  -> HLabel label t (H context)
hlabel labeler a = HLabel $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {} -> LabelSpec (labeler (hfield a field))
{-# INLINABLE hlabel #-}


hunlabel :: HTable t
  => (forall labels necessity nullability blueprint. ()
    => context ('Spec (label ': labels) necessity nullability blueprint)
    -> context ('Spec labels necessity nullability blueprint))
  -> HLabel label t (H context)
  -> t (H context)
hunlabel unlabler (HLabel as) =
  htabulate $ \field -> case hfield as field of
      LabelSpec a -> unlabler a
{-# INLINABLE hunlabel #-}
