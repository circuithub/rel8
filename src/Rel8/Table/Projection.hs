{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Projection
  ( Projection
  , Projectable( project )
  , Biprojectable( biproject )
  , Projecting
  , apply
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Field ( Field( Field ), fields )
import Rel8.Schema.HTable ( hfield, htabulate )
import Rel8.Table ( Columns, Context, Transpose, toColumns )
import Rel8.Table.Transpose ( Transposes )


-- | The constraint @'Projecting' a b@ ensures that @'Projection' a b@ is a
-- usable 'Projection'.
type Projecting :: Type -> Type -> Constraint
class
  ( Transposes (Field a) (Context a) a (Transpose (Field a) a)
  , Transposes (Field a) (Context a) b (Transpose (Field a) b)
  )
  => Projecting a b
instance
  ( Transposes (Field a) (Context a) a (Transpose (Field a) a)
  , Transposes (Field a) (Context a) b (Transpose (Field b) b)
  )
  => Projecting a b


-- | A @'Projection' a b@s is a special type of function @a -> b@ whereby the
-- resulting @b@ is guaranteed to be composed only from columns contained in
-- @a@.
type Projection :: Type -> Type -> Type
type Projection a b = Transpose (Field a) a -> Transpose (Field a) b


-- | @'Projectable' f@ means that @f@ is a kind of functor on 'Rel8.Table's
-- that allows the mapping of a 'Projection' over its underlying columns.
type Projectable :: (Type -> Type) -> Constraint
class Projectable f where
  -- | Map a 'Projection' over @f@.
  project :: Projecting a b
    => Projection a b -> f a -> f b


-- | @'Biprojectable' p@ means that @p@ is a kind of bifunctor on
-- 'Rel8.Table's that allows the mapping of a pair of 'Projection's  over its
-- underlying columns.
type Biprojectable :: (Type -> Type -> Type) -> Constraint
class Biprojectable p where
  -- | Map a pair of 'Projection's over @p@.
  biproject :: (Projecting a b, Projecting c d)
    => Projection a b -> Projection c d -> p a c -> p b d


apply :: Projecting a b
  => Projection a b -> Columns a context -> Columns b context
apply f a = case toColumns (f fields) of
  bs -> htabulate $ \field -> case hfield bs field of
    Field field' -> hfield a field'
