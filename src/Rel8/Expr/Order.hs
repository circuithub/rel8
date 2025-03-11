{-# language DataKinds #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Order
  ( asc
  , desc
  , nullsFirst
  , nullsLast
  )
where

-- base
import Data.Bifunctor ( first )
import Prelude

-- opaleye
import Opaleye.Internal.HaskellDB.PrimQuery ( OrderOp( orderDirection, orderNulls ) )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Order as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( unsafeUnnullify )
import Rel8.Expr.Opaleye ( toPrimExpr )
import Rel8.Order ( Order( Order ) )
import Rel8.Type.Ord ( DBOrd )


-- | Sort a column in ascending order.
asc :: DBOrd a => Order (Expr a)
asc = Order $ Opaleye.Order (\expr -> [(orderOp, toPrimExpr expr)])
  where
    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpAsc
      , orderNulls = Opaleye.NullsLast
      }


-- | Sort a column in descending order.
desc :: DBOrd a => Order (Expr a)
desc = Order $ Opaleye.Order (\expr -> [(orderOp, toPrimExpr expr)])
  where
    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpDesc
      , orderNulls = Opaleye.NullsFirst
      }


-- | Transform an ordering so that @null@ values appear first. This corresponds
-- to @NULLS FIRST@ in SQL.
nullsFirst :: Order (Expr a) -> Order (Expr (Maybe a))
nullsFirst (Order (Opaleye.Order f)) =
  Order $ Opaleye.Order $ fmap (first g) . f . unsafeUnnullify
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsFirst }


-- | Transform an ordering so that @null@ values appear last. This corresponds
-- to @NULLS LAST@ in SQL.
nullsLast :: Order (Expr a) -> Order (Expr (Maybe a))
nullsLast (Order (Opaleye.Order f)) =
  Order $ Opaleye.Order $ fmap (first g) . f . unsafeUnnullify
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsLast }
