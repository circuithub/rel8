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
  , take, takeExpr
  , drop, dropExpr
  , elem

    -- ** @NonEmptyTable@
  , NonEmptyTable
  , head1, head1Expr
  , index1, index1Expr
  , last1, last1Expr
  , length1, length1Expr
  , take1, take1Expr
  , drop1, drop1Expr
  , elem1

    -- ** Unsafe
  , unsafeSubscript
  , unsafeSubscripts
  )
where

-- base
import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (drop, elem, head, last, length, take)

-- rel8
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Array (listOf, nonEmptyOf)
import Rel8.Internal.Expr.Function (rawBinaryOperator)
import Rel8.Internal.Expr.List
import Rel8.Internal.Expr.NonEmpty
import Rel8.Internal.Expr.Subscript
import Rel8.Internal.Schema.Null (Sql)
import Rel8.Internal.Table.List
import Rel8.Internal.Table.NonEmpty
import Rel8.Internal.Type.Eq (DBEq)


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
