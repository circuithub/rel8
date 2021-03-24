{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
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
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Nullify
  ( HNullify( HNullify )
  , Nullify
  , hnulls, hnullify, hunnullify
  )
where

-- base
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.HTable
  ( HTable
  , hfield, htabulate, htabulateA, hspecs
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )

-- semigroupoids
import Data.Functor.Apply ( Apply )
import Rel8.Schema.HTable.MapTable
import Rel8.FCF
import GHC.Generics (Generic)


type HNullify :: K.HTable -> K.HTable
newtype HNullify table context = HNullify (HMapTable Nullify table context)
  deriving stock Generic
  deriving anyclass HTable



-- | Transform a 'Spec' by allowing it to be @null@.
data Nullify :: Spec -> Exp Spec


type instance Eval (Nullify ('Spec labels necessity dbType _)) = 
  'Spec labels necessity dbType (Maybe dbType)


instance MapSpec Nullify where
  mapInfo = \case
    SSpec{labels, necessity, info, nullability} -> SSpec
      { labels
      , necessity
      , info
      , nullability = case nullability of
          Nullable    -> Nullable
          NonNullable -> Nullable
      } 


hnulls :: HTable t
  => (forall labels necessity dbType. ()
    => context ('Spec labels necessity dbType (Maybe dbType)))
  -> HNullify t context
hnulls null = HNullify $ htabulate $ \(HMapTableField field) -> case hfield hspecs field of
  SSpec {} -> null
{-# INLINABLE hnulls #-}


hnullify :: HTable t
  => (forall labels necessity dbType a. ()
    => SSpec ('Spec labels necessity dbType a)
    -> context ('Spec labels necessity dbType a)
    -> context ('Spec labels necessity dbType (Maybe dbType)))
  -> t context
  -> HNullify t context
hnullify nullifier a = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@SSpec {} -> nullifier spec (hfield a field)
{-# INLINABLE hnullify #-}


hunnullify :: (HTable t, Apply m)
  => (forall labels necessity dbType a. ()
    => SSpec ('Spec labels necessity dbType a)
    -> context ('Spec labels necessity dbType (Maybe dbType))
    -> m (context ('Spec labels necessity dbType a)))
  -> HNullify t context
  -> m (t context)
hunnullify unnullifier (HNullify as) =
  htabulateA $ \field -> case hfield hspecs field of
    spec@SSpec {} -> case hfield as (HMapTableField field) of
      a -> unnullifier spec a
{-# INLINABLE hunnullify #-}
