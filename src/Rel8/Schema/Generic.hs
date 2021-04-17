{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Schema.Generic
  ( Rel8able
  , KRel8able
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

-- rel8
import Rel8.Generic.Record ( Record(..) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.Field ( Reify, Reifiable, hreify, hunreify )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , GTable, GColumns, fromGColumns, toGColumns
  )


instance
  ( Rel8able t
  , Labelable context
  , Reifiable context
  ) => Table context (t context)
 where
  type Columns (t context) = GRep t
  type Context (t context) = context

  fromColumns = unreify . gfromColumns . hreify
  toColumns = hunreify . gtoColumns . reify


type KRel8able :: Type
type KRel8able = K.Table


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
type Rel8able :: KRel8able -> Constraint
class HTable (GRep t) => Rel8able t where
  gfromColumns :: (Labelable context, Reifiable context)
    => GRep t (Col (Reify context)) -> t (Reify context)

  gtoColumns :: (Labelable context, Reifiable context)
    => t (Reify context) -> GRep t (Col (Reify context))

  default gfromColumns ::
    ( Generic (Record (t (Reify context)))
    , GColumns (Rep (Record (t (Reify context)))) ~ GRep t
    , GTable (Reify context) (Rep (Record (t (Reify context))))
    ) => GRep t (Col (Reify context)) -> t (Reify context)
  gfromColumns = unrecord . to . fromGColumns

  default gtoColumns ::
    ( Generic (Record (t (Reify context)))
    , GColumns (Rep (Record (t (Reify context)))) ~ GRep t
    , GTable (Reify context) (Rep (Record (t (Reify context))))
    ) => t (Reify context) -> GRep t (Col (Reify context))
  gtoColumns = toGColumns . from . Record


type GRep :: K.Table -> K.HTable
type GRep t = GColumns (Rep (Record (t (Reify Name))))


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
