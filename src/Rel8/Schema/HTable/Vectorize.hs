{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Vectorize
  ( HVectorize
  , hvectorize, hvectorizeA, hunvectorize
  , hnullify
  , happend, hempty
  , hproject
  , htraverseVectorP
  , hcolumn
  , First (..)
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.Semigroup as Base
import GHC.Generics (Generic)
import Prelude

-- product-profunctors
import Data.Profunctor.Product (ProductProfunctor)

-- profunctors
import Data.Profunctor (dimap)

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Schema.Dict ( Dict( Dict ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable
  ( HField, HTable, hfield, htabulate, htabulateA, hspecs
  , htraversePWithField
  )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.HTable.MapTable
  ( HMapTable( HMapTable ), HMapTableField( HMapTableField )
  , MapSpec, mapInfo
  , Precompose( Precompose )
  )
import qualified Rel8.Schema.HTable.MapTable as HMapTable ( hproject )
import Rel8.Schema.HTable.Nullify (HNullify (HNullify))
import Rel8.Schema.Null (Nullify, Unnullify, NotNull, Nullity (NotNull))
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Type.Array ( listTypeInformation, nonEmptyTypeInformation )
import Rel8.Type.Information ( TypeInformation )

-- semialign
import Data.Align (Semialign, alignWith)
import Data.Zip (Unzip, Zip, Zippy(..), zipWith)

-- semigroupoids
import Data.Functor.Apply (Apply)


type Vector :: (Type -> Type) -> Constraint
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


hvectorizeA :: (HTable t, Apply f, Vector list)
  => (forall a. Spec a -> HField t a -> f (context' (list a)))
  -> f (HVectorize list t context')
hvectorizeA vectorizer = fmap HVectorize $
  htabulateA $ \(HMapTableField field) ->
    case hfield hspecs field of
      spec -> vectorizer spec field
{-# INLINABLE hvectorizeA #-}


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


htraverseVectorP :: (HTable t, ProductProfunctor p)
  => (forall a. HField t a -> p (f (list a)) (g (list' a)))
  -> p (HVectorize list t f) (HVectorize list' t g)
htraverseVectorP f =
  dimap (\(HVectorize (HMapTable a)) -> a) (HVectorize . HMapTable) $
    htraversePWithField $ \field ->
      dimap (\(Precompose a) -> a) Precompose (f field)


hcolumn :: HVectorize list (HIdentity a) context -> context (list a)
hcolumn (HVectorize (HMapTable (HIdentity (Precompose a)))) = a


hnullify :: forall t list context. (HTable t, Vector list)
  => (forall a. Spec a -> context (list a) -> context (Nullify a))
  -> HVectorize list t context
  -> HNullify t context
hnullify f (HVectorize table) = HNullify $
  htabulate $ \(HMapTableField field) -> case hfield hspecs field of
    spec -> case hfield table (HMapTableField field) of
      a -> f spec a


newtype First a b = First {getFirst :: a}
  deriving stock Functor
  deriving (Semigroup) via (Base.First a)


instance Semialign (First a) where
  alignWith _ (First a) _ = First a


instance Zip (First a) where
  zipWith _ (First a) _ = First a