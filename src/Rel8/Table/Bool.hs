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
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( boolExpr, caseExpr )
import Rel8.Schema.Context ( DB( DB ), unDB )
import Rel8.Schema.HTable ( htabulate, hfield )
import Rel8.Table ( Table, fromColumns, toColumns )


bool :: Table DB a => a -> a -> Expr nullability Bool -> a
bool (toColumns -> false) (toColumns -> true) condition =
  fromColumns $ htabulate $ \field ->
    case (hfield false field, hfield true field) of
      (DB falseExpr, DB trueExpr) ->
        DB (boolExpr falseExpr trueExpr condition)


case_ :: Table DB a => [(Expr nullability Bool, a)] -> a -> a
case_ (map (fmap toColumns) -> branches) (toColumns -> fallback) =
  fromColumns $ htabulate $ \field -> case hfield fallback field of
    DB fallbackExpr ->
      case map (fmap (unDB . (`hfield` field))) branches of
        branchExprs -> DB (caseExpr branchExprs fallbackExpr)
