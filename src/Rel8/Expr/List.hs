{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Expr.List (
  headExpr,
  indexExpr,
  lastExpr,
  sheadExpr,
  sindexExpr,
  slastExpr,
  lengthExpr,
) where

-- base
import Data.Int (Int32)
import Prelude

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (mapPrimExpr, toPrimExpr)
import Rel8.Schema.Null (Nullify, Sql, Unnullify)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Information (TypeInformation)
import qualified Rel8.Type.Array as Prim


headExpr :: Sql DBType a => Expr [a] -> Expr (Nullify a)
headExpr = sheadExpr typeInformation


indexExpr :: Sql DBType a => Expr Int32 -> Expr [a] -> Expr (Nullify a)
indexExpr = sindexExpr typeInformation


lastExpr :: Sql DBType a => Expr [a] -> Expr (Nullify a)
lastExpr = slastExpr typeInformation


sheadExpr :: TypeInformation (Unnullify a) -> Expr [a] -> Expr (Nullify a)
sheadExpr info = mapPrimExpr (Prim.head info)


sindexExpr :: TypeInformation (Unnullify a) -> Expr Int32 -> Expr [a] -> Expr (Nullify a)
sindexExpr info i = mapPrimExpr (Prim.index info (toPrimExpr i))


slastExpr :: TypeInformation (Unnullify a) -> Expr [a] -> Expr (Nullify a)
slastExpr info = mapPrimExpr (Prim.last info)


lengthExpr :: Expr [a] -> Expr Int32
lengthExpr = mapPrimExpr (Prim.length)
