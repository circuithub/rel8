{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Transpose
  ( Transposes
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- base-compat
import Data.Type.Equality.Compat

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Table ( Table, Transpose, Congruent )


-- | @'Transposes' from to a b@ means that @a@ and @b@ are 'Table's, in the
-- @from@ and @to@ contexts respectively, which share the same underlying
-- structure. In other words, @b@ is a version of @a@ transposed from the
-- @from@ context to the @to@ context (and vice versa).
type Transposes :: K.Context -> K.Context -> Type -> Type -> Constraint
class
  ( Table from a
  , Table to b
  , Congruent a b
  , b ~ Transpose to a
  , a ~ Transpose from b
  )
  => Transposes from to a b
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
  )
  => Transposes from to a b
