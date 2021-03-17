{-# options_ghc -Wno-redundant-constraints #-}

module Rel8.Expr.Bool
  ( (&&.)
  , and_
  , (||.)
  , or_
  , not_
  ) where

-- base
import Data.Foldable ( foldl' )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Expr ( Expr )
import Rel8.Expr.Lit ( litExpr )
import Rel8.Expr.Opaleye ( mapPrimExpr, zipPrimExprsWith )


-- | The SQL @AND@ operator.
--
-- >>> :{
-- mapM_ print =<< select c do
--   x <- values [lit True, lit False]
--   y <- values [lit True, lit False]
--   return (x, y, x &&. y)
-- :}
-- (True,True,True)
-- (True,False,False)
-- (False,True,False)
-- (False,False,False)
infixr 3 &&.


(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(&&.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpAnd)


-- | Fold @AND@ over a collection of expressions.
--  
-- >>> select c $ pure $ and_ [ lit True ==. lit False, lit False, lit True ]
-- [False]
--  
-- >>> select c $ pure $ and_ []
-- [True]
and_ :: Foldable f => f (Expr Bool) -> Expr Bool
and_ = foldl' (&&.) (litExpr True)


-- | The SQL @OR@ operator.
--
-- >>> :{
-- mapM_ print =<< select c do
--   x <- values [lit True, lit False]
--   y <- values [lit True, lit False]
--   return (x, y, x ||. y)
-- :}
-- (True,True,True)
-- (True,False,True)
-- (False,True,True)
-- (False,False,False)
infixr 2 ||.


(||.) :: Expr Bool -> Expr Bool -> Expr Bool
(||.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpOr)


-- | Fold @OR@ over a collection of expressions.
-- 
-- >>> select c $ pure $ or_ [ lit True ==. lit False, lit False, lit True ]
-- [True]
--  
-- >>> select c $ pure $ or_ []
-- [False]
or_ :: Foldable f => f (Expr Bool) -> Expr Bool
or_ = foldl' (||.) (litExpr False)


-- | The SQL @NOT@ operator.
--
-- >>> select c $ pure $ not_ $ lit True
-- [False]
--
-- >>> select c $ pure $ not_ $ lit False
-- [True]
not_ :: Expr Bool -> Expr Bool
not_ = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNot)
