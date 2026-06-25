{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rel8.Internal.Expr.Range (
  range,  
  multirange,
) where

-- rel8
import Rel8.Internal.Data.Range (
  Range, mapRange, quoteRange,
  Multirange, primMultirange,
 )
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Opaleye (fromPrimExpr, toPrimExpr)
import Rel8.Internal.Type.Range (DBRange, rangeTypeName, multirangeTypeName)


range :: forall a. DBRange a => Range (Expr a) -> Expr (Range a)
range = fromPrimExpr . quoteRange name . mapRange toPrimExpr
  where
    name = rangeTypeName @a


multirange :: forall a. DBRange a => [Expr (Range a)] -> Expr (Multirange a)
multirange = fromPrimExpr . primMultirange name . map toPrimExpr
  where
    name = multirangeTypeName @a
