{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}

{-# options_ghc -Wno-simplifiable-class-constraints #-}

module Rel8.Query.Order
  ( Order(..)
  , orderBy
  , asc
  , desc
  , nullsFirst
  , nullsLast
  , distinctOnBy
  ) where

-- base
import Control.Applicative ( Const( Const ), getConst )
import Data.Bifunctor ( first )
import Data.Functor.Contravariant ( Contravariant )

-- contravariant
import Data.Functor.Contravariant.Divisible ( Decidable, Divisible )

-- rel8
import Opaleye.Internal.HaskellDB.PrimQuery ( OrderOp( orderDirection, orderNulls ) )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Order as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Lateral as Opaleye
import qualified Opaleye.Order as Opaleye ( orderBy )
import Rel8.Expr ( Expr( Expr ), retype )
import Rel8.HTable ( htraverse )
import Rel8.Info ( HasInfo )
import Rel8.Query ( Query, liftOpaleye, mapOpaleye, toOpaleye )
import Rel8.Table ( Table, toColumns )
import Rel8.Table.Opaleye ( unpackspec )


-- | An ordering expression for @a@. Primitive orderings are defined with 'asc'
-- and 'desc', and you can combine @Order@ via its various instances.
--
-- A common pattern is to use '<>' to combine multiple orderings in sequence,
-- and '>$<' (from 'Contravariant') to select individual columns. For example,
-- to sort a @Query@ on two columns, we could do:
--
-- >>> import Data.Functor.Contravariant ((>$<))
-- >>> :{
-- select c $ orderBy (mconcat [fst >$< asc, snd >$< desc]) $ do
--   x <- values [ lit x | x <- [1..3 :: Int32 ] ]
--   y <- values [ lit x | x <- [1..3 :: Int32 ] ]
--   return (x, y)
-- :}
-- [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
newtype Order a = Order (Opaleye.Order a)
  deriving newtype (Contravariant, Divisible, Decidable, Semigroup, Monoid)


-- | Sort a column in ascending order.
--
-- >>> select c $ orderBy asc $ values [ lit x | x <- [1..5 :: Int32] ]
-- [1,2,3,4,5]
asc :: HasInfo a => Order (Expr a)
asc = Order $ Opaleye.Order (getConst . htraverse f . toColumns)
  where
    f :: forall x. Expr x -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (Expr x)
    f (Expr primExpr) = Const [(orderOp, primExpr)]

    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp
      { orderDirection = Opaleye.OpAsc
      , orderNulls = Opaleye.NullsLast
      }


-- | Sort a column in descending order.
--
-- >>> select c $ orderBy desc $ values [ lit x | x <- [1..5 :: Int32] ]
-- [5,4,3,2,1]
desc :: HasInfo a => Order (Expr a)
desc = Order $ Opaleye.Order (getConst . htraverse f . toColumns)
  where
    f :: forall x. Expr x -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (Expr x)
    f (Expr primExpr) = Const [(orderOp, primExpr)]

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
nullsFirst (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f . retype
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsFirst }


-- | Transform an ordering so that @null@ values appear first. This corresponds
-- to @NULLS LAST@ in SQL.
--
-- >>> select c $ orderBy (nullsLast desc) $ values $ [ nullExpr, nullExpr ] <> [ lit (Just x) | x <- [1..5 :: Int32] ]
-- [Just 5,Just 4,Just 3,Just 2,Just 1,Nothing,Nothing]
nullsLast :: Order (Expr a) -> Order (Expr (Maybe a))
nullsLast (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f . retype
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsLast }


-- | Order the rows returned by a 'Query' according to a particular 'Order'.
--
-- For an example of using this, see the documentation for 'Order'.
orderBy :: Order a -> Query a -> Query a
orderBy (Order o) = liftOpaleye . Opaleye.laterally (Opaleye.orderBy o) . toOpaleye


distinctOnBy :: Table Expr b => (a -> b) -> Order a -> Query a -> Query a
distinctOnBy proj (Order order) =
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOnBy unpackspec proj order . Opaleye.runSimpleQueryArr q))
