{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Rel8able
  ( KRel8able, Rel8able
  , Algebra
  , GRep
  , GColumns, gfromColumns, gtoColumns
  , GFromExprs, gfromResult, gtoResult
  , TSerialize, serialize, deserialize
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.Type.Bool ( type (&&) )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.FCF ( Exp, Eval )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Generic.Table ( GAlgebra )
import qualified Rel8.Generic.Table.Record as G
import qualified Rel8.Kind.Algebra as K ( Algebra(..) )
import Rel8.Kind.Context ( SContext(..) )
import Rel8.Schema.Field ( Field )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  , TTable, TColumns
  )
import Rel8.Table.Transpose ( Transposes )


-- | The kind of 'Rel8able' types
type KRel8able :: Type
type KRel8able = K.Rel8able


-- This is almost 'Data.Type.Equality.==', but we add an extra case.
type (==) :: k -> k -> Bool
type family a == b where
  -- This extra case is needed to solve the equation "a == Identity a", 
  -- which occurs when we have polymorphic Rel8ables 
  -- (e.g., newtype T a f = T { x :: Column f a })
  a == Identity a = 'False
  
  -- These cases are exactly the same as those in 'Data.Type.Equality.==.
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False


type Serialize :: Bool -> Type -> Type -> Constraint
class transposition ~ (a == Transpose Result expr) =>
  Serialize transposition expr a
 where
  serialize :: a -> Columns expr Result
  deserialize :: Columns expr Result -> a


instance
  ( (a == Transpose Result expr) ~ 'True
  , Transposes Expr Result expr a
  )
  => Serialize 'True expr a
 where
  serialize = toColumns
  deserialize = fromColumns


instance
  ( (a == Transpose Result expr) ~ 'False
  , Table (Context expr) expr
  , FromExprs expr ~ a
  )
  => Serialize 'False expr a
 where
  serialize = toResult @_ @expr
  deserialize = fromResult @_ @expr


data TSerialize :: Type -> Type -> Exp Constraint
type instance Eval (TSerialize expr a) =
  Serialize (a == Transpose Result expr) expr a


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
  type GColumns t :: K.HTable
  type GFromExprs t :: Type

  gfromColumns :: SContext context -> GColumns t context -> t context
  gtoColumns :: SContext context -> t context -> GColumns t context

  gfromResult :: GColumns t Result -> GFromExprs t
  gtoResult :: GFromExprs t -> GColumns t Result

  type GColumns t = G.GColumns TColumns (GRep t Expr)
  type GFromExprs t = t Result

  default gfromColumns :: forall context.
    ( SRel8able t Aggregate
    , SRel8able t Expr
    , forall table. SRel8able t (Field table)
    , SRel8able t Name
    , SSerialize t
    )
    => SContext context -> GColumns t context -> t context
  gfromColumns = \case
    SAggregate -> sfromColumns
    SExpr -> sfromColumns
    SField -> sfromColumns
    SName -> sfromColumns
    SResult -> sfromResult

  default gtoColumns :: forall context.
    ( SRel8able t Aggregate
    , SRel8able t Expr
    , forall table. SRel8able t (Field table)
    , SRel8able t Name
    , SSerialize t
    )
    => SContext context -> t context -> GColumns t context
  gtoColumns = \case
    SAggregate -> stoColumns
    SExpr -> stoColumns
    SField -> stoColumns
    SName -> stoColumns
    SResult -> stoResult

  default gfromResult :: (SSerialize t, GFromExprs t ~ t Result)
    => GColumns t Result -> GFromExprs t
  gfromResult = sfromResult

  default gtoResult :: (SSerialize t, GFromExprs t ~ t Result)
    => GFromExprs t -> GColumns t Result
  gtoResult = stoResult


type Algebra :: K.Rel8able -> K.Algebra
type Algebra t = GAlgebra (GRep t Expr)


type GRep :: K.Rel8able -> K.Context -> Type -> Type
type GRep t context = Rep (Record (t context))


type SRel8able :: K.Rel8able -> K.Context -> Constraint
class
  ( Generic (Record (t context))
  , G.GTable (TTable context) TColumns (GRep t context)
  , G.GColumns TColumns (GRep t context) ~ GColumns t
  )
  => SRel8able t context
instance
  ( Generic (Record (t context))
  , G.GTable (TTable context) TColumns (GRep t context)
  , G.GColumns TColumns (GRep t context) ~ GColumns t
  )
  => SRel8able t context


type SSerialize :: K.Rel8able -> Constraint
type SSerialize t =
  ( Generic (Record (t Result))
  , G.GSerialize TSerialize TColumns (GRep t Expr) (GRep t Result)
  , G.GColumns TColumns (GRep t Expr) ~ GColumns t
  )


sfromColumns :: forall t context. SRel8able t context
  => GColumns t context -> t context
sfromColumns =
  unrecord .
  to .
  G.gfromColumns @(TTable context) @TColumns fromColumns


stoColumns :: forall t context. SRel8able t context
  => t context -> GColumns t context
stoColumns =
  G.gtoColumns @(TTable context) @TColumns toColumns .
  from .
  Record


sfromResult :: forall t. SSerialize t
  => GColumns t Result -> t Result
sfromResult =
  unrecord .
  to .
  G.gfromResult
    @TSerialize
    @TColumns
    @(GRep t Expr)
    @(GRep t Result)
    (\(_ :: proxy x) -> deserialize @_ @x)


stoResult :: forall t. SSerialize t
  => t Result -> GColumns t Result
stoResult =
  G.gtoResult
    @TSerialize
    @TColumns
    @(GRep t Expr)
    @(GRep t Result)
    (\(_ :: proxy x) -> serialize @_ @x) .
  from .
  Record
