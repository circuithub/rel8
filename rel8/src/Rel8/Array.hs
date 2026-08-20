{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rel8.Array
  (
    -- ** @ListTable@
    ListTable
  , head, headExpr
  , index, indexExpr
  , last, lastExpr
  , length, lengthExpr
  , elem, notElem

    -- ** @NonEmptyTable@
  , NonEmptyTable
  , head1, head1Expr
  , index1, index1Expr
  , last1, last1Expr
  , length1, length1Expr
  , elem1, notElem1

    -- ** Unsafe
  , unsafeSubscript
  , unsafeSubscripts
  )
where

-- base
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (elem, head, last, length, notElem)

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Bool (not_)
import Rel8.Internal.Expr.Function (rawFunction)
import Rel8.Internal.Expr.List
import Rel8.Internal.Expr.NonEmpty
import Rel8.Internal.Expr.Null (isNonNull, isNull)
import Rel8.Internal.Expr.Opaleye (fromPrimExpr, toPrimExpr)
import Rel8.Internal.Expr.Subscript
import Rel8.Internal.Schema.Null (Nullity (NotNull, Null), Sql, nullable)
import Rel8.Internal.Table.List
import Rel8.Internal.Table.NonEmpty
import Rel8.Internal.Type (DBType)
import Rel8.Internal.Type.Eq (DBEq)


-- | @'elem' a as@ tests whether @a@ is an element of the list @as@.
elem :: Sql DBEq a => Expr a -> Expr [a] -> Expr Bool
elem = memberOf
infix 4 `elem`


-- | @'elem1' a as@ tests whether @a@ is an element of the non-empty list
-- @as@.
elem1 :: Sql DBEq a => Expr a -> Expr (NonEmpty a) -> Expr Bool
elem1 = memberOf
infix 4 `elem1`


-- | @'notElem' a as@ tests whether @a@ is not an element of the list @as@.
notElem :: Sql DBEq a => Expr a -> Expr [a] -> Expr Bool
notElem = notMemberOf
infix 4 `notElem`


-- | @'notElem1' a as@ tests whether @a@ is not an element of the non-empty
-- list @as@.
notElem1 :: Sql DBEq a => Expr a -> Expr (NonEmpty a) -> Expr Bool
notElem1 = notMemberOf
infix 4 `notElem1`


memberOf :: forall a t. (Sql DBEq a, DBType (t a))
  => Expr a -> Expr (t a) -> Expr Bool
memberOf = case nullable @a of
  Null -> \ma mas -> isNonNull (position mas ma)
  NotNull -> eqAny


notMemberOf :: forall a t. (Sql DBEq a, DBType (t a))
  => Expr a -> Expr (t a) -> Expr Bool
notMemberOf = case nullable @a of
  Null -> \ma mas -> isNull (position mas ma)
  NotNull -> \a as -> not_ (eqAny a as)


position :: (DBType (t (Maybe a)), DBType a)
  => Expr (t (Maybe a)) -> Expr (Maybe a) -> Expr (Maybe Int32)
position as a = rawFunction "array_position" (as, a)


eqAny :: Expr a -> Expr as -> Expr Bool
eqAny a as =
  fromPrimExpr (Opaleye.AnyExpr (Opaleye.:==) (toPrimExpr a) (toPrimExpr as))
