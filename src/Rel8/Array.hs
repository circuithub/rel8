{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Rel8.Array
  (
    -- ** @ListTable@
    ListTable
  , head, headExpr
  , index, indexExpr
  , last, lastExpr
  , length, lengthExpr
  , elem

    -- ** @NonEmptyTable@
  , NonEmptyTable
  , head1, head1Expr
  , index1, index1Expr
  , last1, last1Expr
  , length1, length1Expr
  , elem1

    -- ** Unsafe
  , unsafeSubscript
  , unsafeSubscripts
  )
where

-- base
import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (elem, head, last, length)

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Array (listOf, nonEmptyOf)
import Rel8.Expr.Function (rawBinaryOperator)
import Rel8.Expr.List
import Rel8.Expr.NonEmpty
import Rel8.Expr.Subscript
import Rel8.Schema.Null (Sql)
import Rel8.Table.List
import Rel8.Table.NonEmpty
import Rel8.Type.Eq (DBEq)


-- | @'elem' a as@ tests whether @a@ is an element of the list @as@.
elem :: Sql DBEq a => Expr a -> Expr [a] -> Expr Bool
elem = (<@) . listOf . pure
  where
    (<@) = rawBinaryOperator "<@"
infix 4 `elem`


-- | @'elem1' a as@ tests whether @a@ is an element of the non-empty list
-- @as@.
elem1 :: Sql DBEq a => Expr a -> Expr (NonEmpty a) -> Expr Bool
elem1 = (<@) . nonEmptyOf . pure
  where
    (<@) = rawBinaryOperator "<@"
infix 4 `elem1`
