{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Vectorize
  ( HVectorize
  , hvectorize, hunvectorize
  , happend, hempty
  , hrelabel
  )
where

-- base
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Schema.Context.Label ( HLabelable, hlabeler, hunlabeler )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable
  ( HTable
  , hfield, htabulate, htabulateA, hspecs
  )
import Rel8.Schema.Nullability ( IsMaybe, Nullability( NonNullable ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Type.Array ( listTypeInformation, nonEmptyTypeInformation )
import Rel8.Type.Information ( TypeInformation )

-- semialign
import Data.Zip ( Unzip, Repeat, Zippy(..) )
import Rel8.FCF
import Rel8.Schema.HTable.MapTable
import GHC.Generics (Generic)


class Vector list where
  listIsn'tMaybe :: proxy a -> IsMaybe (list a) :~: 'False
  vectorTypeInformation :: ()
    => Nullability a ma
    -> TypeInformation a
    -> TypeInformation (list ma)


instance Vector [] where
  listIsn'tMaybe _ = Refl
  vectorTypeInformation = listTypeInformation


instance Vector NonEmpty where
  listIsn'tMaybe _ = Refl
  vectorTypeInformation = nonEmptyTypeInformation


type HVectorize :: (Type -> Type) -> K.HTable -> K.HTable
newtype HVectorize list table context = HVectorize (HMapTable (Vectorize list) table context)
  deriving stock Generic
  deriving anyclass HTable


data Vectorize :: (Type -> Type) -> Spec -> Exp Spec


type instance Eval (Vectorize list ('Spec labels necessity _ a)) = 'Spec labels necessity (list a) (list a)


instance Vector list => MapSpec (Vectorize list) where
  mapInfo = \case
    SSpec {..} -> case listIsn'tMaybe @list nullability of
      Refl -> SSpec
        { nullability = NonNullable
        , info = vectorTypeInformation nullability info
        , ..
        }


hvectorize :: (HTable t, Unzip f, Vector list)
  => (forall labels necessity db a. ()
    => SSpec ('Spec labels necessity db a)
    -> f (context ('Spec labels necessity db a))
    -> context' ('Spec labels necessity (list a) (list a)))
  -> f (t context)
  -> HVectorize list t context'
hvectorize vectorizer as = HVectorize $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@SSpec {} -> vectorizer spec (fmap (`hfield` field) as)
{-# INLINABLE hvectorize #-}


hunvectorize :: (HTable t, Repeat f, Vector list)
  => (forall labels necessity db a. ()
    => SSpec ('Spec labels necessity db a)
    -> context ('Spec labels necessity (list a) (list a))
    -> f (context' ('Spec labels necessity db a)))
  -> HVectorize list t context
  -> f (t context')
hunvectorize unvectorizer (HVectorize table) =
  getZippy $ htabulateA $ \field -> case hfield hspecs field of
    spec@SSpec{} -> case hfield table (HMapTableField field) of
      a -> Zippy (unvectorizer spec a)
{-# INLINABLE hunvectorize #-}


happend :: (HTable t, Vector list) =>
  ( forall labels necessity db a. ()
    => Nullability db a
    -> TypeInformation db
    -> context ('Spec labels necessity (list a) (list a))
    -> context ('Spec labels necessity (list a) (list a))
    -> context ('Spec labels necessity (list a) (list a))
  )
  -> HVectorize list t context
  -> HVectorize list t context
  -> HVectorize list t context
happend append (HVectorize as) (HVectorize bs) = HVectorize $
  htabulate $ \field@(HMapTableField j) -> case (hfield as field, hfield bs field) of
    (a, b) -> case hfield hspecs j of
      SSpec {nullability, info} -> append nullability info a b


hempty :: HTable t =>
  ( forall labels necessity db a. ()
    => Nullability db a
    -> TypeInformation db
    -> context ('Spec labels necessity [a] [a])
  )
  -> HVectorize [] t context
hempty empty = HVectorize $ htabulate $ \(HMapTableField field) -> case hfield hspecs field of
  SSpec {nullability, info} -> empty nullability info


instance HLabelable g => HLabelable (Precompose (Vectorize list) g) where
  hlabeler = Precompose . hlabeler . precomposed
  hunlabeler = Precompose . hunlabeler . precomposed


hrelabel :: HLabelable context
  => (forall ctx. HLabelable ctx => t ctx -> u ctx)
  -> HVectorize list t context
  -> HVectorize list u context
hrelabel f (HVectorize (HMapTable table)) = HVectorize (HMapTable (f table))
