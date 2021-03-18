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
import Rel8.Expr.Null ( Nullification, unsafeUnnullify )
import Rel8.Expr.Opaleye ( unsafeToPrimExpr )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Order ( Order( Order ) )
import Rel8.Type.Ord ( DBOrd )


asc :: DBOrd a => Order (Expr 'NonNullable a)
asc = Order $ Opaleye.Order (\expr -> [(orderOp, unsafeToPrimExpr expr)])
  where
    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpAsc
      , orderNulls = Opaleye.NullsLast
      }


desc :: DBOrd a => Order (Expr 'NonNullable a)
desc = Order $ Opaleye.Order (\expr -> [(orderOp, unsafeToPrimExpr expr)])
  where
    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpDesc
      , orderNulls = Opaleye.NullsFirst
      }


nullsFirst :: Nullification nonNullable nullable
  => Order (Expr nonNullable a) -> Order (Expr nullable a)
nullsFirst (Order (Opaleye.Order f)) =
  Order $ Opaleye.Order $ fmap (first g) . f . unsafeUnnullify
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsFirst }


nullsLast :: Nullification nonNullable nullable
  => Order (Expr nonNullable a) -> Order (Expr nullable a)
nullsLast (Order (Opaleye.Order f)) =
  Order $ Opaleye.Order $ fmap (first g) . f . unsafeUnnullify
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsLast }
