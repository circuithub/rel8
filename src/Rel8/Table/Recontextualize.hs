{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.Recontextualize
  ( Recontextualize
  , Aggregates
  , Encodes
  , Selects
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate, Col( Aggregation ) )
import Rel8.Expr ( Expr )
import Rel8.Opaque ( Opaque, Opaque1 )
import Rel8.Schema.Context ( Insertion, Name )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( KnownSpec )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Table ( Table, Congruent )
import Rel8.Type ( DBType )


type Recontextualize :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Constraint
class
  ( Table from a
  , Table to b
  , Congruent a b
  , Recontextualize from from a a
  , Recontextualize to to b b
  , Recontextualize to from b a
  ) => Recontextualize from to a b
    | a -> from
    , b -> to
    , a to -> b
    , b from -> a


instance Sql DBType a =>
  Recontextualize Aggregate Aggregate (Aggregate (Expr a)) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Aggregate Expr (Aggregate (Expr a)) (Expr a)


instance Sql DBType a =>
  Recontextualize Aggregate Identity (Aggregate (Expr a)) (Identity a)


instance Sql DBType a =>
  Recontextualize Expr Aggregate (Expr a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Expr Expr (Expr a) (Expr a)


instance Sql DBType a =>
  Recontextualize Expr Identity (Expr a) (Identity a)


instance Sql DBType a =>
  Recontextualize Identity Aggregate (Identity a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Identity Expr (Identity a) (Expr a)


instance Sql DBType a =>
  Recontextualize Identity Identity (Identity a) (Identity a)


instance Recontextualize from to (Opaque1 from a) (Opaque1 to a)


instance HTable t => Recontextualize from to (t (Col from)) (t (Col to))


instance
  ( Table from (t from)
  , Table to (t to)
  , Congruent (t from) (t to)
  ) => Recontextualize from to (t from) (t to)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Labelable from
  , Labelable to
  ) => Recontextualize from to (a1, a2) (b1, b2)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  , Labelable from, Labelable to
  ) => Recontextualize from to (a1, a2, a3) (b1, b2, b3)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  , Recontextualize from to a4 b4
  , Labelable from, Labelable to
  ) => Recontextualize from to (a1, a2, a3, a4) (b1, b2, b3, b4)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  , Recontextualize from to a4 b4
  , Recontextualize from to a5 b5
  , Labelable from, Labelable to
  ) => Recontextualize from to (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)


type Aggregates :: Type -> Type -> Constraint
class Recontextualize Aggregate Expr aggregates exprs => Aggregates aggregates exprs
instance Recontextualize Aggregate Expr aggregates exprs => Aggregates aggregates exprs
instance {-# OVERLAPPING #-} Aggregates (Opaque1 Aggregate Opaque) (Opaque1 Expr Opaque)


type Encodes :: Type -> Type -> Constraint
class Recontextualize Identity Expr a exprs => Encodes a exprs
instance Recontextualize Identity Expr a exprs => Encodes a exprs
instance {-# OVERLAPPING #-} Encodes (Opaque1 Identity Opaque) (Opaque1 Expr Opaque)


type Selects :: Type -> Type -> Constraint
class Recontextualize Name Expr names exprs => Selects names exprs
instance Recontextualize Name Expr names exprs => Selects names exprs
instance {-# OVERLAPPING #-} Selects (Opaque1 Name Opaque) (Opaque1 Expr Opaque)
