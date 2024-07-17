{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Expr.NonEmpty (
  head1Expr,
  index1Expr,
  last1Expr,
  shead1Expr,
  sindex1Expr,
  slast1Expr,
  length1Expr,
) where

-- base
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Prelude

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (mapPrimExpr, toPrimExpr)
import Rel8.Schema.Null (Nullify, Sql, Unnullify)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Information (TypeInformation)
import qualified Rel8.Type.Array as Prim


head1Expr :: Sql DBType a => Expr (NonEmpty a) -> Expr a
head1Expr = shead1Expr typeInformation


index1Expr :: Sql DBType a => Expr Int32 -> Expr (NonEmpty a) -> Expr (Nullify a)
index1Expr = sindex1Expr typeInformation


last1Expr :: Sql DBType a => Expr (NonEmpty a) -> Expr a
last1Expr = slast1Expr typeInformation


shead1Expr :: TypeInformation (Unnullify a) -> Expr (NonEmpty a) -> Expr a
shead1Expr info = mapPrimExpr (Prim.head info)


sindex1Expr :: TypeInformation (Unnullify a) -> Expr Int32 -> Expr (NonEmpty a) -> Expr (Nullify a)
sindex1Expr info i = mapPrimExpr (Prim.index info (toPrimExpr i))


slast1Expr :: TypeInformation (Unnullify a) -> Expr (NonEmpty a) -> Expr a
slast1Expr info = mapPrimExpr (Prim.last info)


length1Expr :: Expr (NonEmpty a) -> Expr Int32
length1Expr = mapPrimExpr (Prim.length)
