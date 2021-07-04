{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Nullify
  ( HNullify( HNullify )
  , Nullify
  , hguard
  , hnulls
  , hnullify
  , hunnullify
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude hiding ( null )

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Schema.HTable ( HTable, hfield, htabulate, htabulateA, hspecs )
import Rel8.Schema.HTable.MapTable
  ( HMapTable, HMapTableField( HMapTableField )
  , MapSpec, mapInfo
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import qualified Rel8.Schema.Null as Type ( Nullify )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )

-- semigroupoids
import Data.Functor.Apply ( Apply )


type HNullify :: K.HTable -> K.HTable
newtype HNullify table context = HNullify (HMapTable Nullify table context)
  deriving stock Generic
  deriving anyclass HTable


-- | Transform a 'Spec' by allowing it to be @null@.
data Nullify :: Spec -> Exp Spec


type instance Eval (Nullify ('Spec a)) =
  'Spec (Type.Nullify a)


instance MapSpec Nullify where
  mapInfo = \case
    SSpec {nullity, ..} -> SSpec
      { nullity = case nullity of
          Null    -> Null
          NotNull -> Null
      , ..
      }


hguard :: HTable t
  => (forall a. context ('Spec (Maybe a)) -> context ('Spec (Maybe a)))
  -> HNullify t context -> HNullify t context
hguard guarder (HNullify as) = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    SSpec {nullity} -> case hfield as (HMapTableField field) of
      a -> case nullity of
        Null -> guarder a
        NotNull -> guarder a


hnulls :: HTable t
  => (forall a. ()
    => SSpec ('Spec a)
    -> context ('Spec (Type.Nullify a)))
  -> HNullify t context
hnulls null = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@SSpec {} -> null spec
{-# INLINABLE hnulls #-}


hnullify :: HTable t
  => (forall a. ()
    => SSpec ('Spec a)
    -> context ('Spec a)
    -> context ('Spec (Type.Nullify a)))
  -> t context
  -> HNullify t context
hnullify nullifier a = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@SSpec {} -> nullifier spec (hfield a field)
{-# INLINABLE hnullify #-}


hunnullify :: (HTable t, Apply m)
  => (forall a. ()
    => SSpec ('Spec a)
    -> context ('Spec (Type.Nullify a))
    -> m (context ('Spec a)))
  -> HNullify t context
  -> m (t context)
hunnullify unnullifier (HNullify as) =
  htabulateA $ \field -> case hfield hspecs field of
    spec@SSpec {} -> case hfield as (HMapTableField field) of
      a -> unnullifier spec a
{-# INLINABLE hunnullify #-}
