module Rel8.Expr.Bool
  ( false, true
  , (&&.), (||.), not_
  , and_, or_
  , boolExpr
  , caseExpr
  , coalesce
  )
where

-- base
import Data.Foldable ( foldl' )
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye ( mapPrimExpr, zipPrimExprsWith )
import Rel8.Expr.Serialize ( litExpr )


-- | The SQL @false@ literal.
false :: Expr Bool
false = litExpr False


-- | The SQL @true@ literal.
true :: Expr Bool
true = litExpr True


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
(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(&&.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpAnd)
infixr 3 &&.


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
(||.) :: Expr Bool -> Expr Bool -> Expr Bool
(||.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpOr)
infixr 2 ||.


-- | The SQL @NOT@ operator.
--
-- >>> select c $ pure $ not_ $ lit True
-- [False]
--
-- >>> select c $ pure $ not_ $ lit False
-- [True]
not_ :: Expr Bool -> Expr Bool
not_ = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNot)


-- | Fold @AND@ over a collection of expressions.
--  
-- >>> select c $ pure $ and_ [ lit True ==. lit False, lit False, lit True ]
-- [False]
--  
-- >>> select c $ pure $ and_ []
-- [True]
and_ :: Foldable f => f (Expr Bool) -> Expr Bool
and_ = foldl' (&&.) true


-- | Fold @OR@ over a collection of expressions.
-- 
-- >>> select c $ pure $ or_ [ lit True ==. lit False, lit False, lit True ]
-- [True]
--  
-- >>> select c $ pure $ or_ []
-- [False]
or_ :: Foldable f => f (Expr Bool) -> Expr Bool
or_ = foldl' (||.) false


-- | Eliminate a boolean-valued expression.
--
-- Corresponds to 'Data.Bool.bool'.
boolExpr :: Expr a -> Expr a -> Expr Bool -> Expr a
boolExpr ifFalse ifTrue condition = caseExpr [(condition, ifTrue)] ifFalse


-- | A multi-way if/then/else statement. The first argument to @caseExpr@ is a
-- list of alternatives. The first alternative that is of the form @(true, x)@
-- will be returned. If no such alternative is found, a fallback expression is
-- returned.
--
-- Corresponds to a @CASE@ expression in SQL.
caseExpr :: [(Expr Bool, Expr a)] -> Expr a -> Expr a
caseExpr branches (Expr fallback) =
  Expr $ Opaleye.CaseExpr (map go branches) fallback
  where
    go (Expr condition, Expr value) = (condition, value)


coalesce :: Expr (Maybe Bool) -> Expr Bool
coalesce (Expr a) = Expr a &&. Expr (Opaleye.FunExpr "COALESCE" [a, untrue])
  where
    untrue = Opaleye.ConstExpr (Opaleye.BoolLit False)
