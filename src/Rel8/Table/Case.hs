{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

module Rel8.Table.Case
  ( Case
  , case_
  , undefined
  )
where

-- base
import Prelude hiding ( undefined )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( caseExpr )
import Rel8.Expr.Null ( snull, unsafeUnnullify )
import Rel8.Schema.HTable ( hfield, htabulate, hspecs )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Table ( Table, fromColumns, toColumns )


class Case a where
  -- | Produce a table expression from a list of alternatives. Returns the
  -- first table where the @Expr Bool@ expression is @True@. If no
  -- alternatives are true, the given default is returned.
  case_ :: [(Expr Bool, a)] -> a -> a

  undefined :: a


instance {-# INCOHERENT #-} Table Expr a => Case a where
  case_ (map (fmap toColumns) -> branches) (toColumns -> fallback) =
    fromColumns $ htabulate $ \field -> case hfield fallback field of
      fallbackExpr ->
        case map (fmap (`hfield` field)) branches of
          branchExprs -> caseExpr branchExprs fallbackExpr
  undefined = fromColumns $ htabulate $ \field -> case hfield hspecs field of
    Spec {nullity, info} -> case nullity of
      Null -> snull info
      NotNull -> unsafeUnnullify (snull info)


instance Case b => Case (a -> b) where
  case_ branches fallback a = case_ (map (fmap ($ a)) branches) (fallback a)
  undefined = const undefined
