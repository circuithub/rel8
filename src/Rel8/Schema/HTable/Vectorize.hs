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
  , hproject
  , hcolumn
  )
where

-- base
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics (Generic)
import Prelude

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Schema.Dict ( Dict( Dict ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HTable, hfield, htabulate, htabulateA, hspecs )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.HTable.MapTable
  ( HMapTable( HMapTable ), HMapTableField( HMapTableField )
  , MapSpec, mapInfo
  , Precompose( Precompose )
  )
import qualified Rel8.Schema.HTable.MapTable as HMapTable ( hproject )
import Rel8.Schema.Null ( Unnullify, NotNull, Nullity( NotNull ) )
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Type.Array ( listTypeInformation, nonEmptyTypeInformation )
import Rel8.Type.Information ( TypeInformation )

-- semialign
import Data.Zip ( Unzip, Zip, Zippy(..) )


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


data Vectorize :: (Type -> Type) -> Type -> Exp Type


type instance Eval (Vectorize list a) = list a


instance Vector list => MapSpec (Vectorize list) where
  mapInfo = \case
    Spec {..} -> case listNotNull @list nullity of
      Dict -> Spec
        { nullity = NotNull
        , info = vectorTypeInformation nullity info
        , ..
        }


hvectorize :: (HTable t, Unzip f, Vector list)
  => (forall a. Spec a -> f (context a) -> context' (list a))
  -> f (t context)
  -> HVectorize list t context'
hvectorize vectorizer as = HVectorize $ htabulate $ \(HMapTableField field) ->
  case hfield hspecs field of
    spec -> vectorizer spec (fmap (`hfield` field) as)
{-# INLINABLE hvectorize #-}


hunvectorize :: (HTable t, Zip f, Vector list)
  => (forall a. Spec a -> context (list a) -> f (context' a))
  -> HVectorize list t context
  -> f (t context')
hunvectorize unvectorizer (HVectorize table) =
  getZippy $ htabulateA $ \field -> case hfield hspecs field of
    spec -> case hfield table (HMapTableField field) of
      a -> Zippy (unvectorizer spec a)
{-# INLINABLE hunvectorize #-}


happend :: (HTable t, Vector list)
  => (forall a. Spec a -> context (list a) -> context (list a) -> context (list a))
  -> HVectorize list t context
  -> HVectorize list t context
  -> HVectorize list t context
happend append (HVectorize as) (HVectorize bs) = HVectorize $
  htabulate $ \field@(HMapTableField j) -> case (hfield as field, hfield bs field) of
    (a, b) -> case hfield hspecs j of
      spec -> append spec a b


hempty :: HTable t
  => (forall a. Spec a -> context [a])
  -> HVectorize [] t context
hempty empty = HVectorize $ htabulate $ \(HMapTableField field) ->
  empty (hfield hspecs field)


hproject :: ()
  => (forall ctx. t ctx -> t' ctx)
  -> HVectorize list t context -> HVectorize list t' context
hproject f (HVectorize a) = HVectorize (HMapTable.hproject f a)


hcolumn :: HVectorize list (HIdentity a) context -> context (list a)
hcolumn (HVectorize (HMapTable (HIdentity (Precompose a)))) = a
