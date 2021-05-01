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

module Rel8.Generic.Rel8able
  ( KRel8able, Rel8able
  , Algebra
  , GRep
  , GColumns, GContext, gfromColumns, gtoColumns
  , GColumnsADT, gfromColumnsADT, gtoColumnsADT
  , greify, gunreify
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude

-- rel8
import Rel8.Kind.Context ( Reifiable )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Map ( GMap, GMappable, gmap, gunmap )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Generic.Table
  ( GGTable, GGColumns, GGContext, ggfromColumns, ggtoColumns
  , GAlgebra
  )
import qualified Rel8.Generic.Table.ADT as G
import Rel8.Kind.Algebra
  ( SAlgebra( SProduct, SSum )
  , KnownAlgebra, algebraSing
  )
import qualified Rel8.Kind.Algebra as K ( Algebra(..) )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Col( Reify ), Reify, UnwrapReify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( fromColumns, toColumns, reify, unreify
  , TTable, TColumns, TContext, TUnreify
  )


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
class
  ( HTable (GColumns t)
  , HTable (GColumnsADT t)
  , GContext t Result ~ Result
  )
  => Rel8able t
 where
  type Algebra t :: K.Algebra

  type GRep t (context :: K.Context) :: Type -> Type

  type GColumns t :: K.HTable

  type GContext t (context :: K.Context) :: K.Context

  gfromColumns :: (Labelable context, Reifiable context, GContext t context ~ context)
    => GColumns t (Col (Reify context)) -> t (Reify context)

  gtoColumns :: (Labelable context, Reifiable context, GContext t context ~ context)
    => t (Reify context) -> GColumns t (Col (Reify context))

  gfromColumnsADT :: GColumnsADT t (Col (Reify Result)) -> t (Reify Result)

  gtoColumnsADT :: t (Reify Result) -> GColumnsADT t (Col (Reify Result))

  greify :: (Labelable context, Reifiable context)
    => t context -> t (Reify context)

  gunreify :: (Labelable context, Reifiable context)
    => t (Reify context) -> t context

  type Algebra t = GAlgebra (GRep t (Reify Name))

  type GRep t context = Rep (Record (t context))

  type GColumns t = Eval (GGColumns (Algebra t) TColumns (GRep t (Reify Name)))

  type GContext t context =
    Eval (GGContext (Algebra t) TUnreifyContext (GRep t (Reify context)))

  default gfromColumns :: forall context.
    ( Generic (Record (t (Reify context)))
    , GRep t (Reify context) ~ Rep (Record (t (Reify context)))
    , KnownAlgebra (Algebra t)
    , Eval (GGTable (Algebra t) (TTable (Reify context)) TColumns (Col (Reify context)) (GRep t (Reify context)))
    , Eval (GGColumns (Algebra t) TColumns (GRep t (Reify context))) ~ GColumns t
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
    , GRep t (Reify context) ~ Rep (Record (t (Reify context)))
    , KnownAlgebra (Algebra t)
    , Eval (GGTable (Algebra t) (TTable (Reify context)) TColumns (Col (Reify context)) (GRep t (Reify context)))
    , Eval (GGColumns (Algebra t) TColumns (GRep t (Reify context))) ~ GColumns t
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

  default gfromColumnsADT ::
    ( Generic (Record (t (Reify Result)))
    , GRep t (Reify Result) ~ Rep (Record (t (Reify Result)))
    , G.GTableADT (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRep t (Reify Result))
    , G.GColumnsADT TColumns (GRep t (Reify Result)) ~ GColumnsADT t
    )
    => GColumnsADT t (Col (Reify Result)) -> t (Reify Result)
  gfromColumnsADT =
    unrecord .
    to .
    G.gfromColumnsADT
      @(TTable (Reify Result))
      @TColumns
      (\(Reify a) -> a)
      Reify
      fromColumns

  default gtoColumnsADT ::
    ( Generic (Record (t (Reify Result)))
    , GRep t (Reify Result) ~ Rep (Record (t (Reify Result)))
    , G.GTableADT (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRep t (Reify Result))
    , G.GColumnsADT TColumns (GRep t (Reify Result)) ~ GColumnsADT t
    )
    => t (Reify Result) -> GColumnsADT t (Col (Reify Result))
  gtoColumnsADT =
    G.gtoColumnsADT
      @(TTable (Reify Result))
      @TColumns
      (\(Reify a) -> a)
      Reify
      toColumns .
    from .
    Record

  default greify :: forall context.
    ( Generic (Record (t context))
    , GRep t context ~ Rep (Record (t context))
    , Generic (Record (t (Reify context)))
    , GRep t (Reify context) ~ Rep (Record (t (Reify context)))
    , GMappable (TTable (Reify context)) (GRep t (Reify context))
    , GRep t context ~ GMap TUnreify (GRep t (Reify context))
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
    , GRep t context ~ Rep (Record (t context))
    , Generic (Record (t (Reify context)))
    , GRep t (Reify context) ~ Rep (Record (t (Reify context)))
    , GMappable (TTable (Reify context)) (GRep t (Reify context))
    , GRep t context ~ GMap TUnreify (GRep t (Reify context))
    )
    => t (Reify context) -> t context
  gunreify =
    unrecord .
    to .
    gmap @(TTable (Reify context)) (Proxy @TUnreify) (unreify Refl) .
    from .
    Record


type GColumnsADT t = G.GColumnsADT TColumns (GRep t (Reify Name))


data TUnreifyContext :: Type -> Exp K.Context
type instance Eval (TUnreifyContext a) = UnwrapReify (Eval (TContext a))


type Constraints :: K.Rel8able -> K.Context -> Constraint
type Constraints t context = Constraints' (Algebra t) t context


type Constraints' :: K.Algebra -> K.Rel8able -> K.Context -> Constraint
type family Constraints' algebra t context where
  Constraints' 'K.Product t context =
    Eval (GGContext 'K.Product TContext (GRep t (Reify context))) ~
      Reify context
  Constraints' 'K.Sum _ context = context ~ Result


fromContext :: forall t context spec.
  ( KnownAlgebra (Algebra t)
  , Constraints t context
  )
  => Col (Reify context) spec
  -> Col (Eval (GGContext (Algebra t) TContext (GRep t (Reify context)))) spec
fromContext = case algebraSing @(Algebra t) of
  SProduct -> id
  SSum -> \(Reify a) -> a


toContext :: forall t context spec.
  ( KnownAlgebra (Algebra t)
  , Constraints t context
  )
  => Col (Eval (GGContext (Algebra t) TContext (GRep t (Reify context)))) spec
  -> Col (Reify context) spec
toContext = case algebraSing @(Algebra t) of
  SProduct -> id
  SSum -> Reify
