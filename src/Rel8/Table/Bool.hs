{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Bool
  ( bool
  , case_
  , nullable
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col( E, unE ) )
import Rel8.Expr.Bool ( boolExpr, caseExpr )
import Rel8.Expr.Null ( isNull, unsafeUnnullify )
import Rel8.Schema.HTable ( htabulate, hfield )
import Rel8.Table ( Table, fromColumns, toColumns )


-- | An if-then-else expression on tables.
--
-- @bool x y p@ returns @x@ if @p@ is @False@, and returns @y@ if @p@ is
-- @True@.
bool :: Table Expr a => a -> a -> Expr Bool -> a
bool (toColumns -> false) (toColumns -> true) condition =
  fromColumns $ htabulate $ \field ->
    case (hfield false field, hfield true field) of
      (E falseExpr, E trueExpr) ->
        E (boolExpr falseExpr trueExpr condition)
{-# INLINABLE bool #-}


-- | Produce a table expression from a list of alternatives. Returns the first
-- table where the @Expr Bool@ expression is @True@. If no alternatives are
-- true, the given default is returned.
case_ :: Table Expr a => [(Expr Bool, a)] -> a -> a
case_ (map (fmap toColumns) -> branches) (toColumns -> fallback) =
  fromColumns $ htabulate $ \field -> case hfield fallback field of
    E fallbackExpr ->
      case map (fmap (unE . (`hfield` field))) branches of
        branchExprs -> E (caseExpr branchExprs fallbackExpr)


-- | Like 'maybe', but to eliminate @null@.
nullable :: Table Expr b => b -> (Expr a -> b) -> Expr (Maybe a) -> b
nullable b f ma = bool (f (unsafeUnnullify ma)) b (isNull ma)
