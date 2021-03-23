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
  , hnulls, hnullify, hunnullify
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htabulateA, htraverse, hdicts, hspecs
  )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec(..) )

-- semigroupoids
import Data.Functor.Apply ( Apply )


type HNullify :: HKTable -> HKTable
data HNullify table context where
  HNullify :: table (H (NullifySpec context)) -> HNullify table (H context)


type HNullifyField :: HKTable -> Context
data HNullifyField table spec where
  HNullifyField
    :: HField table ('Spec labels necessity dbType a)
    -> HNullifyField table ('Spec labels necessity dbType (Maybe dbType))


instance HTable table => HTable (HNullify table) where
  type HField (HNullify table) = HNullifyField table
  type HConstrainTable (HNullify table) c =
    HConstrainTable table (NullifySpecC c)

  hfield (HNullify table) (HNullifyField field) =
    getNullifySpec (hfield table field)

  htabulate f = HNullify $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> NullifySpec (f (HNullifyField field))

  htraverse f (HNullify t) = HNullify <$> htraverse (traverseNullifySpec f) t

  hdicts :: forall c. HConstrainTable table (NullifySpecC c)
    => HNullify table (H (Dict c))
  hdicts = HNullify $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield (hdicts @_ @(NullifySpecC c)) field of
      Dict -> NullifySpec Dict

  hspecs = HNullify $ htabulate $ \field -> case hfield hspecs field of
    SSpec {..} -> case nullability of
      Nullable -> NullifySpec SSpec {nullability = Nullable, ..}
      NonNullable -> NullifySpec SSpec {nullability = Nullable, ..}

  {-# INLINABLE hfield #-}
  {-# INLINABLE htabulate #-}
  {-# INLINABLE htraverse #-}
  {-# INLINABLE hdicts #-}
  {-# INLINABLE hspecs #-}


type NullifyingSpec :: Type -> Type
type NullifyingSpec r = (Spec -> r) -> Spec -> r


type NullifySpec :: NullifyingSpec Type
data NullifySpec context spec where
  NullifySpec
    :: { getNullifySpec :: context ('Spec labels necessity dbType (Maybe dbType)) }
    -> NullifySpec context ('Spec labels necessity dbType a)


type NullifySpecC :: NullifyingSpec Constraint
class
  ( forall labels necessity dbType a.
    ( spec ~ 'Spec labels necessity dbType a =>
       constraint ('Spec labels necessity dbType (Maybe dbType))
    )
  ) => NullifySpecC constraint spec
instance
  ( spec ~ 'Spec labels necessity dbType a
  , constraint ('Spec labels necessity dbType (Maybe dbType))
  ) => NullifySpecC constraint spec


traverseNullifySpec :: forall context context' spec m. Functor m
  => (forall x. context x -> m (context' x))
  -> NullifySpec context spec -> m (NullifySpec context' spec)
traverseNullifySpec f (NullifySpec a) = NullifySpec <$> f a


hnulls :: HTable t
  => (forall labels necessity dbType. ()
    => context ('Spec labels necessity dbType (Maybe dbType)))
  -> HNullify t (H context)
hnulls null = HNullify $ htabulate $ \field -> case hfield hspecs field of
  SSpec {} -> NullifySpec null
{-# INLINABLE hnulls #-}


hnullify :: HTable t
  => (forall labels necessity dbType a. ()
    => SSpec ('Spec labels necessity dbType a)
    -> context ('Spec labels necessity dbType a)
    -> context ('Spec labels necessity dbType (Maybe dbType)))
  -> t (H context)
  -> HNullify t (H context)
hnullify nullifier a = HNullify $ htabulate $ \field ->
  case hfield hspecs field of
    spec@SSpec {} -> NullifySpec (nullifier spec (hfield a field))
{-# INLINABLE hnullify #-}


hunnullify :: (HTable t, Apply m)
  => (forall labels necessity dbType a. ()
    => SSpec ('Spec labels necessity dbType a)
    -> context ('Spec labels necessity dbType (Maybe dbType))
    -> m (context ('Spec labels necessity dbType a)))
  -> HNullify t (H context)
  -> m (t (H context))
hunnullify unnullifier (HNullify as) =
  htabulateA $ \field -> case hfield hspecs field of
    spec@SSpec {} -> case hfield as field of
      NullifySpec a -> unnullifier spec a
{-# INLINABLE hunnullify #-}
