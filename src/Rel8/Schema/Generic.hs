{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Schema.Generic
  ( Rel8able
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics
  ( Generic, Rep, from, to
  , (:*:)( (:*:) ), K1( K1 ), M1( M1 )
  , D, S
  , Meta( MetaSel )
  )
import qualified GHC.Generics as G ( C )
import GHC.TypeLits ( KnownSymbol )
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

-- rel8
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.Field ( Reify, Reifiable, hreify, hunreify )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( KTable )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  )


instance
  ( Rel8able t
  , Labelable context
  , Reifiable context
  , HTable (GRep context t)
  ) => Table context (t context)
 where
  type Columns (t context) = GRep context t
  type Context (t context) = context

  fromColumns = unreify . gfromColumns . hreify
  toColumns = hunreify . gtoColumns . reify


-- | This type class allows you to define custom 'Table's using higher-kinded
-- data types. Higher-kinded data types are data types of the pattern:
--
-- @
-- data MyType f =
--   MyType { field1 :: Column f T1 OR HK1 f
--          , field2 :: Column f T2 OR HK2 f
--          , ...
--          , fieldN :: Column f Tn OR HKn f
--          }
-- @
-- 
-- where @Tn@ is any Haskell type, and @HKn@ is any higher-kinded type.
-- 
-- That is, higher-kinded data are records where all fields in the record are
-- all either of the type @Column f T@ (for any @T@), or are themselves
-- higher-kinded data:
-- 
-- [Nested]
-- 
-- @
-- data Nested f =
--   Nested { nested1 :: MyType f
--          , nested2 :: MyType f
--          }
-- @
-- 
-- The @Rel8able@ type class is used to give us a special mapping operation
-- that lets us change the type parameter @f@.
-- 
-- [Supplying @Rel8able@ instances]
-- 
-- This type class should be derived generically for all table types in your
-- project. To do this, enable the @DeriveAnyType@ and @DeriveGeneric@ language
-- extensions:
-- 
-- @
-- \{\-\# LANGUAGE DeriveAnyClass, DeriveGeneric #-\}
-- 
-- data MyType f = MyType { fieldA :: Column f T }
--   deriving ( GHC.Generics.Generic, Rel8able )
-- @
type Rel8able :: KTable -> Constraint
class Rel8able t where
  gfromColumns :: (Labelable context, Reifiable context)
    => GRep context t (Col (Reify context)) -> t (Reify context)

  gtoColumns :: (Labelable context, Reifiable context)
    => t (Reify context) -> GRep context t (Col (Reify context))

  default gfromColumns :: forall context.
    ( Generic (t (Reify context))
    , GRel8able context (Rep (t (Reify context)))
    ) => GRep context t (Col (Reify context)) -> t (Reify context)
  gfromColumns = to . fromGColumns @_ @(Rep (t (Reify context)))

  default gtoColumns :: forall context.
    ( Generic (t (Reify context))
    , GRel8able context (Rep (t (Reify context)))
    ) => t (Reify context) -> GRep context t (Col (Reify context))
  gtoColumns = toGColumns @_ @(Rep (t (Reify context))) . from


type GRep :: K.Context -> K.Table -> K.HTable
type GRep context t = GColumns (Rep (t (Reify context)))


type GColumns :: (Type -> Type) -> K.HTable
type family GColumns rep where
  GColumns (M1 D _ rep) = GColumns rep
  GColumns (M1 G.C _ rep) = GColumns rep
  GColumns (rep1 :*: rep2) = HPair (GColumns rep1) (GColumns rep2)
  GColumns (M1 S ('MetaSel ('Just label) _ _ _) (K1 _ a)) =
    HLabel label (Columns a)


type GRel8able :: K.Context -> (Type -> Type) -> Constraint
class GRel8able context rep where
  fromGColumns :: GColumns rep (Col (Reify context)) -> rep x
  toGColumns :: rep x -> GColumns rep (Col (Reify context))


instance GRel8able context rep => GRel8able context (M1 D c rep) where
  fromGColumns = M1 . fromGColumns @context @rep
  toGColumns (M1 a) = toGColumns @context @rep a


instance GRel8able context rep => GRel8able context (M1 G.C c rep) where
  fromGColumns = M1 . fromGColumns @context @rep
  toGColumns (M1 a) = toGColumns @context @rep a


instance (GRel8able context rep1, GRel8able context rep2) =>
  GRel8able context (rep1 :*: rep2)
 where
  fromGColumns (HPair a b) =
    fromGColumns @context @rep1 a :*: fromGColumns @context @rep2 b
  toGColumns (a :*: b) =
    HPair (toGColumns @context @rep1 a) (toGColumns @context @rep2 b)


instance
  ( Table (Reify context) a
  , Labelable context
  , KnownSymbol label
  , GColumns (M1 S meta k1) ~ HLabel label (Columns a)
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , k1 ~ K1 i a
  ) => GRel8able context (M1 S meta k1)
 where
  fromGColumns = M1 . K1 . fromColumns . hunlabel unlabeler
  toGColumns (M1 (K1 a)) = hlabel labeler (toColumns a)


reify ::
  (-- Rel8able t
  --, forall necessity a. Coercible (Field context necessity a) (AField context necessity a) => Coercible (t context) (t (Reify context))
  )
  => t context -> t (Reify context)
reify = unsafeCoerce


unreify ::
  (-- Rel8able t
  --, forall necessity a. Coercible (AField context necessity a) (Field context necessity a) => Coercible (t (Reify context)) (t context)
  )
  => t (Reify context) -> t context
unreify = unsafeCoerce
