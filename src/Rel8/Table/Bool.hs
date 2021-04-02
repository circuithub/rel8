{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Bool
  ( bool
  , case_
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Bool ( boolExpr, caseExpr )
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
      (DB falseExpr, DB trueExpr) ->
        DB (boolExpr falseExpr trueExpr condition)
{-# INLINABLE bool #-}


-- | Produce a table expression from a list of alternatives. Returns the first
-- table where the @Expr Bool@ expression is @True@. If no alternatives are
-- true, the given default is returned.
case_ :: Table Expr a => [(Expr Bool, a)] -> a -> a
case_ (map (fmap toColumns) -> branches) (toColumns -> fallback) =
  fromColumns $ htabulate $ \field -> case hfield fallback field of
    DB fallbackExpr ->
      case map (fmap (unDB . (`hfield` field))) branches of
        branchExprs -> DB (caseExpr branchExprs fallbackExpr)
