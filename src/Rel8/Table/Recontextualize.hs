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
  , Inserts
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Opaque ( Opaque, Opaque1 )
import Rel8.Schema.Context ( Aggregation, DB, Insertion, Name, Result )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( KnownSpec )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Table ( Table, Congruent )
import Rel8.Type ( DBType )


type Recontextualize :: Kind.Context -> Kind.Context -> Type -> Type -> Constraint
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
  Recontextualize Aggregation Aggregation (Aggregate (Expr a)) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Aggregation DB (Aggregate (Expr a)) (Expr a)


instance Sql DBType a =>
  Recontextualize Aggregation Result (Aggregate (Expr a)) (Identity a)


instance Sql DBType a =>
  Recontextualize DB Aggregation (Expr a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize DB DB (Expr a) (Expr a)


instance Sql DBType a =>
  Recontextualize DB Result (Expr a) (Identity a)


instance Sql DBType a =>
  Recontextualize Result Aggregation (Identity a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Result DB (Identity a) (Expr a)


instance Sql DBType a =>
  Recontextualize Result Result (Identity a) (Identity a)


instance KnownSpec spec => Recontextualize from to (from spec) (to spec)


instance Recontextualize from to (Opaque1 from a) (Opaque1 to a)


instance HTable t => Recontextualize from to (t (H from)) (t (H to))


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
class Recontextualize Aggregation DB aggregates exprs => Aggregates aggregates exprs
instance Recontextualize Aggregation DB aggregates exprs => Aggregates aggregates exprs
instance {-# OVERLAPPING #-} Aggregates (Opaque1 Aggregation Opaque) (Opaque1 DB Opaque)


type Encodes :: Type -> Type -> Constraint
class Recontextualize Result DB a exprs => Encodes a exprs
instance Recontextualize Result DB a exprs => Encodes a exprs
instance {-# OVERLAPPING #-} Encodes (Opaque1 Result Opaque) (Opaque1 DB Opaque)


type Inserts :: Type -> Type -> Constraint
class Recontextualize DB Insertion exprs inserts => Inserts exprs inserts
instance Recontextualize DB Insertion exprs inserts => Inserts exprs inserts
instance {-# OVERLAPPING #-} Inserts (Opaque1 DB Opaque) (Opaque1 Insertion Opaque)


type Selects :: Type -> Type -> Constraint
class Recontextualize Name DB names exprs => Selects names exprs
instance Recontextualize Name DB names exprs => Selects names exprs
instance {-# OVERLAPPING #-} Selects (Opaque1 Name Opaque) (Opaque1 DB Opaque)
