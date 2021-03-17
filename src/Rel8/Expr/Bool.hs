module Rel8.Expr.Bool
  ( in_
  , (&&.)
  , and_
  , (||.)
  , or_
  , not_
  ) where

-- base
import Data.Foldable ( foldl' )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.DBType.DBEq ( DBEq( eqExprs ) )
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Lit ( litExpr )


-- | Like the SQL @IN@ operator, but implemented by folding over a list with
-- '==.' and '||.'.
--
-- >>> select c $ return $ lit (5 :: Int32) `in_` [ lit x | x <- [1..5] ]
-- [True]
--
-- >>> select c $ return $ lit (42 :: Int32) `in_` [ lit x | x <- [1..5] ]
-- [False]
in_ :: DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> b ||. eqExprs x y) (litExpr False)


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
Expr a &&. Expr b = Expr $ Opaleye.BinExpr Opaleye.OpAnd a b


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
Expr a ||. Expr b = Expr $ Opaleye.BinExpr Opaleye.OpOr a b


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
not_ (Expr a) = Expr $ Opaleye.UnExpr Opaleye.OpNot a


