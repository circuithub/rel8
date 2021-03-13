{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Recontextualize
  ( Recontextualize
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate )
import Rel8.Kind.Nullability ( KnownNullability )
import Rel8.Schema.Context ( Aggregation, DB )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.Spec ( KnownSpec )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Table ( Table, Context, Congruent )
import Rel8.Type ( DBType )


type Recontextualize :: Kind.Context -> Kind.Context -> Type -> Type -> Constraint
class
  ( Table a
  , Table b
  , Context a ~ from
  , Context b ~ to
  , Congruent a b
  ) => Recontextualize from to a b
    | a -> from
    , b -> to
    , a to -> b
    , b from -> a


instance (KnownNullability nullability, DBType a) =>
  Recontextualize DB Aggregation (Expr nullability a) (Aggregate nullability a)


instance (KnownNullability nullability, DBType a) =>
  Recontextualize Aggregation DB (Aggregate nullability a) (Expr nullability a)


instance KnownSpec spec => Recontextualize from to (from spec) (to spec)


instance HTable t => Recontextualize from to (t (H from)) (t (H to))


instance (Recontextualize from to a1 b1, Recontextualize from to a2 b2) =>
  Recontextualize from to (a1, a2) (b1, b2)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  ) => Recontextualize from to (a1, a2, a3) (b1, b2, b3)
