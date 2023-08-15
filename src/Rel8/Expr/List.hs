{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Expr.List (
  headExpr,
  lastExpr,
  sheadExpr,
  slastExpr,
  lengthExpr,
) where

-- base
import Data.Int (Int32)
import Prelude

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (fromPrimExpr, toPrimExpr)
import Rel8.Schema.Null (Nullify, Sql, Unnullify)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Information (TypeInformation)
import qualified Rel8.Type.Array as Prim


headExpr :: Sql DBType a => Expr [a] -> Expr (Nullify a)
headExpr = sheadExpr typeInformation


lastExpr :: Sql DBType a => Expr [a] -> Expr (Nullify a)
lastExpr = slastExpr typeInformation


sheadExpr :: TypeInformation (Unnullify a) -> Expr [a] -> Expr (Nullify a)
sheadExpr info = fromPrimExpr . Prim.head info . toPrimExpr


slastExpr :: TypeInformation (Unnullify a) -> Expr [a] -> Expr (Nullify a)
slastExpr info = fromPrimExpr . Prim.last info . toPrimExpr


lengthExpr :: Expr [a] -> Expr Int32
lengthExpr = fromPrimExpr . Prim.length . toPrimExpr
