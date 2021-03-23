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
--
-- >>> select c $ orderBy asc $ values [ lit x | x <- [1..5 :: Int32] ]
-- [1,2,3,4,5]
asc :: DBOrd a => Order (Expr a)
asc = Order $ Opaleye.Order (\expr -> [(orderOp, toPrimExpr expr)])
  where
    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpAsc
      , orderNulls = Opaleye.NullsLast
      }


-- | Sort a column in descending order.
--
-- >>> select c $ orderBy desc $ values [ lit x | x <- [1..5 :: Int32] ]
-- [5,4,3,2,1]
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
--
-- >>> select c $ orderBy (nullsFirst desc) $ values $ [ nullExpr, nullExpr ] <> [ lit (Just x) | x <- [1..5 :: Int32] ]
-- [Nothing,Nothing,Just 5,Just 4,Just 3,Just 2,Just 1]
nullsFirst :: Order (Expr a) -> Order (Expr (Maybe a))
nullsFirst (Order (Opaleye.Order f)) =
  Order $ Opaleye.Order $ fmap (first g) . f . unsafeUnnullify
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsFirst }


-- | Transform an ordering so that @null@ values appear first. This corresponds
-- to @NULLS LAST@ in SQL.
--
-- >>> select c $ orderBy (nullsLast desc) $ values $ [ nullExpr, nullExpr ] <> [ lit (Just x) | x <- [1..5 :: Int32] ]
-- [Just 5,Just 4,Just 3,Just 2,Just 1,Nothing,Nothing]
nullsLast :: Order (Expr a) -> Order (Expr (Maybe a))
nullsLast (Order (Opaleye.Order f)) =
  Order $ Opaleye.Order $ fmap (first g) . f . unsafeUnnullify
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsLast }
