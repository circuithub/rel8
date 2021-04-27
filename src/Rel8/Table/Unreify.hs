{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

-- | This module implements some machinery for implementing methods of the
-- 'Table' class for a particular special (but important) class of polymorphic
-- @Table@ types.
--
-- This special case is characterised by a @newtype@ wrapper around a bare
-- 'HTable' which is constructed by applying a type family to the polymorphic
-- type variable.
--
-- Examples of this class of @Table@ include @ListTable@ and @NonEmptyTable@.
--
-- The tricky part about implementing @Table@ for these types is 'reify' and
-- 'unreify'. There is no guarantee in general that @'Unreify' a@ is itself
-- a @Table@, let alone a @Table@ with the same 'Columns' as @a@
-- (e.g., @Unreify (AColumn Result Bool) = Bool@, and @Bool@ is not a
-- @Table@)

module Rel8.Table.Unreify
  ( Unreifiable
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Reify ( Reify )
import Rel8.Table ( Table, Columns, Unreify )


type Unreifiable :: K.Context -> Type -> Constraint
type family Unreifiable context a where
  Unreifiable (Reify context) a =
    ( Table context (Unreify a)
    , Columns a ~ Columns (Unreify a)
    )
  Unreifiable _ _ = ()
