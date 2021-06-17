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
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
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
import Rel8.Schema.HTable ( HTable, hfield, htabulate, htabulateA, hspecs )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import qualified Rel8.Schema.Null as Type ( Nullify )
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


type instance Eval (Nullify ('Spec labels a)) =
  'Spec labels (Type.Nullify a)


instance MapSpec Nullify where
  mapInfo = \case
    SSpec{labels, info, nullity} -> SSpec
      { labels
      , info
      , nullity = case nullity of
          Null    -> Null
          NotNull -> Null
      } 


hnulls :: HTable t
  => (forall labels a. ()
    => SSpec ('Spec labels a)
    -> context ('Spec labels (Type.Nullify a)))
  -> HNullify t context
hnulls null = HNullify $ htabulate $ \(HMapTableField field) -> case hfield hspecs field of
  spec@SSpec {} -> null spec
{-# INLINABLE hnulls #-}


hnullify :: HTable t
  => (forall labels a. ()
    => SSpec ('Spec labels a)
    -> context ('Spec labels a)
    -> context ('Spec labels (Type.Nullify a)))
  -> t context
  -> HNullify t context
hnullify nullifier a = HNullify $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@SSpec {} -> nullifier spec (hfield a field)
{-# INLINABLE hnullify #-}


hunnullify :: (HTable t, Apply m)
  => (forall labels a. ()
    => SSpec ('Spec labels a)
    -> context ('Spec labels (Type.Nullify a))
    -> m (context ('Spec labels a)))
  -> HNullify t context
  -> m (t context)
hunnullify unnullifier (HNullify as) =
  htabulateA $ \field -> case hfield hspecs field of
    spec@SSpec {} -> case hfield as (HMapTableField field) of
      a -> unnullifier spec a
{-# INLINABLE hunnullify #-}
