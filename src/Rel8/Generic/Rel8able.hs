{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Generic.Rel8able
  ( KRel8able, Rel8able
  , GColumns, GContext, gfromColumns, gtoColumns, greify, gunreify
  , Algebra
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude

-- rel8
import Rel8.Kind.Context
  ( SContext( SReify )
  , Reifiable, contextSing
  , sLabelable, sReifiable
  )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Map ( GMap, GMappable, gmap, gunmap )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Generic.Table
  ( GGTable, GGColumns, GGContext, ggfromColumns, ggtoColumns
  , GAlgebra
  )
import Rel8.Kind.Algebra
  ( SAlgebra( SProduct, SSum )
  , KnownAlgebra, algebraSing
  )
import qualified Rel8.Kind.Algebra as K ( Algebra(..) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify
  ( Col( Reify ), Reify, hreify, hunreify
  , UnwrapReify
  )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  , TTable, TColumns, TContext, TUnreify
  )


instance
  ( Rel8able t
  , GContext t context ~ context
  , Labelable context
  , Reifiable context
  )
  => Table context (t context)
 where
  type Columns (t context) = GColumns t
  type Context (t context) = context
  type Unreify (t context) = t (UnwrapReify context)

  fromColumns = gunreify . gfromColumns . hreify
  toColumns = hunreify . gtoColumns . greify

  reify Refl = case contextSing @context of
    SReify context -> case sLabelable context of
      Dict -> case sReifiable context of
        Dict -> greify

  unreify Refl = case contextSing @context of
    SReify context -> case sLabelable context of
      Dict -> case sReifiable context of
        Dict -> gunreify


type KRel8able :: Type
type KRel8able = K.Rel8able


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
type Rel8able :: K.Rel8able -> Constraint
class HTable (GColumns t) => Rel8able t where
  type Algebra t :: K.Algebra
  type GColumns t :: K.HTable
  type GContext t (context :: K.Context) :: K.Context

  gfromColumns :: (Labelable context, Reifiable context, GContext t context ~ context)
    => GColumns t (Col (Reify context)) -> t (Reify context)

  gtoColumns :: (Labelable context, Reifiable context, GContext t context ~ context)
    => t (Reify context) -> GColumns t (Col (Reify context))

  greify :: (Labelable context, Reifiable context)
    => t context -> t (Reify context)

  gunreify :: (Labelable context, Reifiable context)
    => t (Reify context) -> t context

  type Algebra t = GAlgebra (Rep (Record (t (Reify Name))))
  type GColumns t = Eval (GGColumns (Algebra t) TColumns (Rep (Record (t (Reify Name)))))
  type GContext t context = Eval (GGContext (Algebra t) TUnreifyContext (Rep (Record (t (Reify context)))))

  default gfromColumns :: forall context.
    ( Generic (Record (t (Reify context)))
    , KnownAlgebra (Algebra t)
    , Eval (GGTable (Algebra t) (TTable (Reify context)) TColumns (Col (Reify context)) (Rep (Record (t (Reify context)))))
    , Eval (GGColumns (Algebra t) TColumns (Rep (Record (t (Reify context))))) ~ GColumns t
    , Constraints t context
    )
    => GColumns t (Col (Reify context)) -> t (Reify context)
  gfromColumns =
    unrecord .
    to .
    ggfromColumns
      @(Algebra t)
      @(TTable (Reify context))
      @TColumns
      @TContext
      (fromContext @t @context)
      (toContext @t @context)
      fromColumns

  default gtoColumns :: forall context.
    ( Generic (Record (t (Reify context)))
    , KnownAlgebra (Algebra t)
    , Eval (GGTable (Algebra t) (TTable (Reify context)) TColumns (Col (Reify context)) (Rep (Record (t (Reify context)))))
    , Eval (GGColumns (Algebra t) TColumns (Rep (Record (t (Reify context))))) ~ GColumns t
    , Constraints t context
    )
    => t (Reify context) -> GColumns t (Col (Reify context))
  gtoColumns =
    ggtoColumns
      @(Algebra t)
      @(TTable (Reify context))
      @TColumns
      @TContext
      (fromContext @t @context)
      (toContext @t @context)
      toColumns .
    from .
    Record

  default greify :: forall context.
    ( Generic (Record (t context))
    , Generic (Record (t (Reify context)))
    , GMappable (TTable (Reify context)) (Rep (Record (t (Reify context))))
    , Rep (Record (t context)) ~ GMap TUnreify (Rep (Record (t (Reify context))))
    )
    => t context -> t (Reify context)
  greify =
    unrecord .
    to .
    gunmap @(TTable (Reify context)) (Proxy @TUnreify) (reify Refl) .
    from .
    Record

  default gunreify :: forall context.
    ( Generic (Record (t context))
    , Generic (Record (t (Reify context)))
    , GMappable (TTable (Reify context)) (Rep (Record (t (Reify context))))
    , Rep (Record (t context)) ~ GMap TUnreify (Rep (Record (t (Reify context))))
    )
    => t (Reify context) -> t context
  gunreify =
    unrecord .
    to .
    gmap @(TTable (Reify context)) (Proxy @TUnreify) (unreify Refl) .
    from .
    Record


data TUnreifyContext :: Type -> Exp K.Context
type instance Eval (TUnreifyContext a) = UnwrapReify (Eval (TContext a))


type Constraints :: K.Rel8able -> K.Context -> Constraint
type Constraints t context = Constraints' (Algebra t) t context


type Constraints' :: K.Algebra -> K.Rel8able -> K.Context -> Constraint
type family Constraints' algebra t context where
  Constraints' 'K.Product t context =
    ( Eval (GGContext 'K.Product TContext (Rep (Record (t (Reify context))))) ~
        Reify context
    )
  Constraints' 'K.Sum _ context = context ~ Result


fromContext :: forall t context spec.
  ( KnownAlgebra (Algebra t)
  , Constraints t context
  )
  => Col (Reify context) spec
  -> Col (Eval (GGContext (Algebra t) TContext (Rep (Record (t (Reify context)))))) spec
fromContext = case algebraSing @(Algebra t) of
  SProduct -> id
  SSum -> \(Reify a) -> a


toContext :: forall t context spec.
  ( KnownAlgebra (Algebra t)
  , Constraints t context
  )
  => Col (Eval (GGContext (Algebra t) TContext (Rep (Record (t (Reify context)))))) spec
  -> Col (Reify context) spec
toContext = case algebraSing @(Algebra t) of
  SProduct -> id
  SSum -> Reify
