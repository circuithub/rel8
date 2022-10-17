{-# language MonoLocalBinds #-}

module Rel8.Table.Bool
  ( bool
  , nullable
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( isNull, unsafeUnnullify )
import Rel8.Table.Case (Case, case_)


-- | An if-then-else expression on tables.
--
-- @bool x y p@ returns @x@ if @p@ is @False@, and returns @y@ if @p@ is
-- @True@.
bool :: Case a => a -> a -> Expr Bool -> a
bool ifFalse ifTrue condition = case_ [(condition, ifTrue)] ifFalse


-- | Like 'maybe', but to eliminate @null@.
nullable :: Case b => b -> (Expr a -> b) -> Expr (Maybe a) -> b
nullable b f ma = bool (f (unsafeUnnullify ma)) b (isNull ma)
