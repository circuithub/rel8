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
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Opaque ( Opaque1 )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( KnownSpec )
import Rel8.Table ( Table, Congruent )
import Rel8.Type ( DBType )


type Recontextualize :: K.Context -> K.Context -> Type -> Type -> Constraint
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


instance KnownSpec spec =>
  Recontextualize from to (Col from spec) (Col to spec)


{-
instance Sql DBType a =>
  Recontextualize Aggregate Insertion (Aggregate (Expr a)) (Insertion a)


instance Sql DBType a => Recontextualize Expr Insertion (Expr a) (Insertion a)
-}


instance Sql DBType a => Recontextualize Identity Identity (Identity a) (Identity a)


{-
instance Sql DBType a =>
  Recontextualize Identity Insertion (Identity a) (Insertion a)


instance Sql DBType a =>
  Recontextualize Insertion Aggregate (Insertion a) (Aggregate (Expr a))


instance Sql DBType a => Recontextualize Insertion Expr (Insertion a) (Expr a)


instance Sql DBType a =>
  Recontextualize Insertion Identity (Insertion a) (Identity a)


instance Sql DBType a => Recontextualize Insertion Insertion (Insertion a) (Insertion a)


instance Sql DBType a => Recontextualize Insertion Name (Insertion a) (Name a)


instance Sql DBType a => Recontextualize Name Insertion (Name a) (Insertion a)
-}


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


{-

-- | @Inserts a b@ means that the columns in @a@ are compatible for inserting
-- with the table @b@.
type Inserts :: Type -> Type -> Constraint
class Recontextualize Expr Insertion exprs inserts => Inserts exprs inserts
instance Recontextualize Expr Insertion exprs inserts => Inserts exprs inserts
instance {-# OVERLAPPING #-} Inserts (Opaque1 Expr Opaque) (Opaque1 Insertion Opaque)
-}
