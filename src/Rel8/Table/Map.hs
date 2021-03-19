{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.Map
  ( MapTable
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
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
import Rel8.Table ( Table, Congruent )
import Rel8.Type ( DBType )


type MapTable :: Kind.Context -> Kind.Context -> Type -> Type -> Constraint
class
  ( Table from a
  , Table to b
  , Congruent a b
  , MapTable from from a a
  , MapTable to to b b
  , MapTable to from b a
  ) => MapTable from to a b
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
  MapTable Aggregation Aggregation (Aggregate (Expr nullability a)) (Aggregate (Expr nullability a))


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  MapTable Aggregation DB (Aggregate (Expr nullability a)) (Expr nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  MapTable Aggregation Result (Aggregate (Expr nullability dbType)) (Value nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  MapTable DB Aggregation (Expr nullability a) (Aggregate (Expr nullability a))


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  MapTable DB DB (Expr nullability a) (Expr nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  MapTable DB Result (Expr nullability dbType) (Value nullability a)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  MapTable Result Aggregation (Value nullability a) (Aggregate (Expr nullability dbType))


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , blueprint ~ FromDBType dbType
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  MapTable Result DB (Value nullability a) (Expr nullability dbType)


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , ToType blueprint ~ a
  , ToDBType blueprint ~ dbType
  , DBType dbType
  ) =>
  MapTable Result Result (Value nullability a) (Value nullability a)


instance KnownSpec spec => MapTable from to (from spec) (to spec)


instance HTable t => MapTable from to (t (H from)) (t (H to))


instance
  ( Table from (t from)
  , Table to (t to)
  , Congruent (t from) (t to)
  ) => MapTable from to (t from) (t to)


instance
  ( MapTable from to a1 b1
  , MapTable from to a2 b2
  , Labelable from
  , Labelable to
  ) => MapTable from to (a1, a2) (b1, b2)


instance
  ( MapTable from to a1 b1
  , MapTable from to a2 b2
  , MapTable from to a3 b3
  , Labelable from, Labelable to
  ) => MapTable from to (a1, a2, a3) (b1, b2, b3)


instance
  ( MapTable from to a1 b1
  , MapTable from to a2 b2
  , MapTable from to a3 b3
  , MapTable from to a4 b4
  , Labelable from, Labelable to
  ) => MapTable from to (a1, a2, a3, a4) (b1, b2, b3, b4)


instance
  ( MapTable from to a1 b1
  , MapTable from to a2 b2
  , MapTable from to a3 b3
  , MapTable from to a4 b4
  , MapTable from to a5 b5
  , Labelable from, Labelable to
  ) => MapTable from to (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
