{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Table.Alternative (
  AltTable ((<|>:)),
  AlternativeTable (emptyTable),
)
where

-- base
import Data.Kind (Constraint, Type)
import Prelude ()

-- rel8
import Rel8.Expr (Expr)
import Rel8.Table (Table)


{- | Like 'Alt' in Haskell. This class is purely a Rel8 concept, and allows you
to take a choice between two tables. See also 'AlternativeTable'.

For example, using '<|>:' on 'Rel8.MaybeTable' allows you to combine two
tables and to return the first one that is a "just" MaybeTable.
-}
type AltTable :: (Type -> Type) -> Constraint
class AltTable f where
  -- | An associative binary operation on 'Table's.
  (<|>:) :: Table Expr a => f a -> f a -> f a


  infixl 3 <|>:


{- | Like 'Alternative' in Haskell, some 'Table's form a monoid on applicative
functors.
-}
type AlternativeTable :: (Type -> Type) -> Constraint
class AltTable f => AlternativeTable f where
  -- | The identity of '<|>:'.
  emptyTable :: Table Expr a => f a
