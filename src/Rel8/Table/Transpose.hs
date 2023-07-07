{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Table.Transpose (
  Transposes,
)
where

-- base
import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (~))
import Prelude ()

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Table (Congruent, Table, Transpose)


{- | @'Transposes' from to a b@ means that @a@ and @b@ are 'Table's, in the
@from@ and @to@ contexts respectively, which share the same underlying
structure. In other words, @b@ is a version of @a@ transposed from the
@from@ context to the @to@ context (and vice versa).
-}
type Transposes :: K.Context -> K.Context -> Type -> Type -> Constraint
class
  ( Table from a
  , Table to b
  , Congruent a b
  , b ~ Transpose to a
  , a ~ Transpose from b
  ) =>
  Transposes from to a b
    | a -> from
    , b -> to
    , a to -> b
    , b from -> a


instance
  ( Table from a
  , Table to b
  , Congruent a b
  , b ~ Transpose to a
  , a ~ Transpose from b
  ) =>
  Transposes from to a b
