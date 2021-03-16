{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.Recontextualize
  ( Recontextualize
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate )
import Rel8.Kind.Blueprint
  ( KnownBlueprint
  , FromDBType, ToDBType
  , FromType, ToType
  )
import Rel8.Kind.Nullability ( KnownNullability )
import Rel8.Schema.Context ( Aggregation, DB, Result )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.Spec ( KnownSpec )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Schema.Value ( Value )
import Rel8.Table ( Table, Context, Congruent )
import Rel8.Type ( DBType )


type Recontextualize :: Kind.Context -> Kind.Context -> Type -> Type -> Constraint
class
  ( Table a
  , Table b
  , Context a ~ from
  , Context b ~ to
  , Congruent a b
  , Recontextualize from from a a
  , Recontextualize to to b b
  , Recontextualize to from b a
  ) => Recontextualize from to a b
    | a -> from
    , b -> to
    , a to -> b
    , b from -> a


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  Recontextualize Aggregation Aggregation (Aggregate nullability a) (Aggregate nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  Recontextualize Aggregation DB (Aggregate nullability a) (Expr nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  Recontextualize Aggregation Result (Aggregate nullability dbType) (Value nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  Recontextualize DB Aggregation (Expr nullability a) (Aggregate nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  Recontextualize DB DB (Expr nullability a) (Expr nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  Recontextualize DB Result (Expr nullability dbType) (Value nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  Recontextualize Result Aggregation (Value nullability a) (Aggregate nullability dbType)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  Recontextualize Result DB (Value nullability a) (Expr nullability dbType)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  Recontextualize Result Result (Value nullability a) (Value nullability a)


instance KnownSpec spec => Recontextualize from to (from spec) (to spec)


instance HTable t => Recontextualize from to (t (H from)) (t (H to))


instance
  ( Table (t from)
  , Table (t to)
  , Context (t from) ~ from
  , Context (t to) ~ to
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
