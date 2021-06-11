{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
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
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Vectorize
  ( HVectorize
  , hvectorize, hunvectorize
  , happend, hempty
  )
where

-- base
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- rel8
import Rel8.Kind.Necessity ( Necessity( Required ), SNecessity( SRequired ) )
import Rel8.Schema.Context.Label ( HLabelable, hlabeler, hunlabeler )
import Rel8.Schema.Dict ( Dict( Dict ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable
  ( HTable
  , hfield, htabulate, htabulateA, hspecs
  )
import Rel8.Schema.Null ( Unnullify, NotNull, Nullity( NotNull ) )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Type.Array ( listTypeInformation, nonEmptyTypeInformation )
import Rel8.Type.Information ( TypeInformation )

-- semialign
import Data.Zip ( Unzip, Repeat, Zippy(..) )
import Rel8.FCF
import Rel8.Schema.HTable.MapTable
import GHC.Generics (Generic)


class Vector list where
  listNotNull :: proxy a -> Dict NotNull (list a)
  vectorTypeInformation :: ()
    => Nullity a
    -> TypeInformation (Unnullify a)
    -> TypeInformation (list a)


instance Vector [] where
  listNotNull _ = Dict
  vectorTypeInformation = listTypeInformation


instance Vector NonEmpty where
  listNotNull _ = Dict
  vectorTypeInformation = nonEmptyTypeInformation


type HVectorize :: (Type -> Type) -> K.HTable -> K.HTable
newtype HVectorize list table context = HVectorize (HMapTable (Vectorize list) table context)
  deriving stock Generic
  deriving anyclass HTable


data Vectorize :: (Type -> Type) -> Spec -> Exp Spec


type instance Eval (Vectorize list ('Spec labels _necessity a)) = 'Spec labels 'Required (list a)


instance Vector list => MapSpec (Vectorize list) where
  mapInfo = \case
    SSpec {..} -> case listNotNull @list nullity of
      Dict -> SSpec
        { necessity = SRequired
        , nullity = NotNull
        , info = vectorTypeInformation nullity info
        , ..
        }


hvectorize :: (HTable t, Unzip f, Vector list)
  => (forall labels necessity a. ()
    => SSpec ('Spec labels necessity a)
    -> f (context ('Spec labels necessity a))
    -> context' ('Spec labels 'Required (list a)))
  -> f (t context)
  -> HVectorize list t context'
hvectorize vectorizer as = HVectorize $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec@SSpec {} -> vectorizer spec (fmap (`hfield` field) as)
{-# INLINABLE hvectorize #-}


hunvectorize :: (HTable t, Repeat f, Vector list)
  => (forall labels necessity a. ()
    => SSpec ('Spec labels necessity a)
    -> context ('Spec labels 'Required (list a))
    -> f (context' ('Spec labels necessity a)))
  -> HVectorize list t context
  -> f (t context')
hunvectorize unvectorizer (HVectorize table) =
  getZippy $ htabulateA $ \field -> case hfield hspecs field of
    spec@SSpec{} -> case hfield table (HMapTableField field) of
      a -> Zippy (unvectorizer spec a)
{-# INLINABLE hunvectorize #-}


happend :: (HTable t, Vector list) =>
  ( forall labels necessity a. ()
    => Nullity a
    -> TypeInformation (Unnullify a)
    -> context ('Spec labels necessity (list a))
    -> context ('Spec labels necessity (list a))
    -> context ('Spec labels necessity (list a))
  )
  -> HVectorize list t context
  -> HVectorize list t context
  -> HVectorize list t context
happend append (HVectorize as) (HVectorize bs) = HVectorize $
  htabulate $ \field@(HMapTableField j) -> case (hfield as field, hfield bs field) of
    (a, b) -> case hfield hspecs j of
      SSpec {nullity, info} -> append nullity info a b


hempty :: HTable t =>
  ( forall labels necessity a. ()
    => Nullity a
    -> TypeInformation (Unnullify a)
    -> context ('Spec labels necessity [a])
  )
  -> HVectorize [] t context
hempty empty = HVectorize $ htabulate $ \(HMapTableField field) -> case hfield hspecs field of
  SSpec {nullity, info} -> empty nullity info


instance HLabelable g => HLabelable (Precompose (Vectorize list) g) where
  hlabeler = Precompose . hlabeler . precomposed
  hunlabeler = Precompose . hunlabeler . precomposed
