{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Schema.HTable.Nullify (
  HNullify (HNullify),
  Nullify,
  hguard,
  hnulls,
  hnullify,
  hunnullify,
  hproject,
)
where

-- base
import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude hiding (null)

-- rel8
import Rel8.FCF (Eval, Exp)
import Rel8.Schema.HTable (HTable, hfield, hspecs, htabulate, htabulateA)
import Rel8.Schema.HTable.MapTable (
  HMapTable,
  HMapTableField (HMapTableField),
  MapSpec,
  mapInfo,
 )
import qualified Rel8.Schema.HTable.MapTable as HMapTable (hproject)
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null (Nullity (NotNull, Null))
import qualified Rel8.Schema.Null as Type (Nullify)
import Rel8.Schema.Spec (Spec (..))

-- semigroupoids
import Data.Functor.Apply (Apply)


type HNullify :: K.HTable -> K.HTable
newtype HNullify table context = HNullify (HMapTable Nullify table context)
  deriving stock (Generic)
  deriving anyclass (HTable)


-- | Transform a 'Type' by allowing it to be @null@.
data Nullify :: Type -> Exp Type


type instance Eval (Nullify a) = Type.Nullify a


instance MapSpec Nullify where
  mapInfo = \case
    Spec{nullity, ..} ->
      Spec
        { nullity = case nullity of
            Null -> Null
            NotNull -> Null
        , ..
        }


hguard ::
  HTable t =>
  (forall a. context (Maybe a) -> context (Maybe a)) ->
  HNullify t context ->
  HNullify t context
hguard guarder (HNullify as) = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    Spec{nullity} -> case hfield as (HMapTableField field) of
      a -> case nullity of
        Null -> guarder a
        NotNull -> guarder a


hnulls ::
  HTable t =>
  (forall a. Spec a -> context (Type.Nullify a)) ->
  HNullify t context
hnulls null = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@Spec{} -> null spec
{-# INLINEABLE hnulls #-}


hnullify ::
  HTable t =>
  (forall a. Spec a -> context a -> context (Type.Nullify a)) ->
  t context ->
  HNullify t context
hnullify nullifier a = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@Spec{} -> nullifier spec (hfield a field)
{-# INLINEABLE hnullify #-}


hunnullify ::
  (HTable t, Apply m) =>
  (forall a. Spec a -> context (Type.Nullify a) -> m (context a)) ->
  HNullify t context ->
  m (t context)
hunnullify unnullifier (HNullify as) =
  htabulateA $ \field -> case hfield hspecs field of
    spec@Spec{} -> case hfield as (HMapTableField field) of
      a -> unnullifier spec a
{-# INLINEABLE hunnullify #-}


hproject ::
  () =>
  (forall ctx. t ctx -> t' ctx) ->
  HNullify t context ->
  HNullify t' context
hproject f (HNullify a) = HNullify (HMapTable.hproject f a)
