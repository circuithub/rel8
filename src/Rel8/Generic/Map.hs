{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Map
  ( GMap, GMappable (gmap, gunmap)
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics
  ( (:+:)( L1, R1 ), (:*:)( (:*:) ), K1( K1 ), M1( M1 ), U1( U1 ), V1
  )
import Prelude ()

-- rel8
import Rel8.FCF ( Eval, Exp )


type GMap :: (Type -> Exp Type) -> (Type -> Type) -> Type -> Type
type family GMap f rep where
  GMap f (M1 i c rep) = M1 i c (GMap f rep)
  GMap _ V1 = V1
  GMap f (rep1 :+: rep2) = GMap f rep1 :+: GMap f rep2
  GMap _ U1 = U1
  GMap f (rep1 :*: rep2) = GMap f rep1 :*: GMap f rep2
  GMap f (K1 i a) = K1 i (Eval (f a))


type GMappable :: (Type -> Exp Constraint) -> (Type -> Type) -> Constraint
class GMappable constraint rep where
  gmap :: ()
    => proxy f
    -> (forall a. Eval (constraint a) => a -> Eval (f a))
    -> rep x
    -> GMap f rep x

  gunmap :: ()
    => proxy f
    -> (forall a. Eval (constraint a) => Eval (f a) -> a)
    -> GMap f rep x
    -> rep x


instance GMappable constraint rep => GMappable constraint (M1 i c rep) where
  gmap proxy f (M1 a) = M1 (gmap @constraint proxy f a)
  gunmap proxy f (M1 a) = M1 (gunmap @constraint proxy f a)


instance GMappable constraint V1 where
  gmap _ _ = \case
  gunmap _ _ = \case


instance (GMappable constraint rep1, GMappable constraint rep2) =>
  GMappable constraint (rep1 :+: rep2)
 where
  gmap proxy f = \case
    L1 a -> L1 (gmap @constraint proxy f a)
    R1 a -> R1 (gmap @constraint proxy f a)
  gunmap proxy f = \case
    L1 a -> L1 (gunmap @constraint proxy f a)
    R1 a -> R1 (gunmap @constraint proxy f a)


instance GMappable constraint U1 where
  gmap _ _ U1 = U1
  gunmap _ _ U1 = U1


instance (GMappable constraint rep1, GMappable constraint rep2) =>
  GMappable constraint (rep1 :*: rep2)
 where
  gmap proxy f (a :*: b) =
    gmap @constraint proxy f a :*: gmap @constraint proxy f b
  gunmap proxy f (a :*: b) =
    gunmap @constraint proxy f a :*: gunmap @constraint proxy f b


instance Eval (constraint a) => GMappable constraint (K1 i a) where
  gmap _ f (K1 a) = K1 (f a)
  gunmap _ f (K1 a) = K1 (f a)
