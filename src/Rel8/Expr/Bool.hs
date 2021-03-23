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


false :: Expr Bool
false = litExpr False


true :: Expr Bool
true = litExpr True


(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(&&.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpAnd)
infixr 3 &&.


(||.) :: Expr Bool -> Expr Bool -> Expr Bool
(||.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpOr)
infixr 2 ||.


not_ :: Expr Bool -> Expr Bool
not_ = mapPrimExpr (Opaleye.UnExpr Opaleye.OpNot)


and_ :: Foldable f => f (Expr Bool) -> Expr Bool
and_ = foldl' (&&.) true


or_ :: Foldable f => f (Expr Bool) -> Expr Bool
or_ = foldl' (||.) false


boolExpr :: Expr a -> Expr a -> Expr Bool -> Expr a
boolExpr ifFalse ifTrue condition = caseExpr [(condition, ifTrue)] ifFalse


caseExpr :: [(Expr Bool, Expr a)] -> Expr a -> Expr a
caseExpr branches (Expr fallback) =
  Expr $ Opaleye.CaseExpr (map go branches) fallback
  where
    go (Expr condition, Expr value) = (condition, value)


coalesce :: Expr (Maybe Bool) -> Expr Bool
coalesce (Expr a) = Expr a &&. Expr (Opaleye.FunExpr "COALESCE" [a, untrue])
  where
    untrue = Opaleye.ConstExpr (Opaleye.BoolLit False)
