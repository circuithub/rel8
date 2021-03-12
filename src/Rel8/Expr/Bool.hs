{-# language DataKinds #-}

module Rel8.Expr.Bool
  ( false, true
  , (&&.), (||.), not_
  , boolExpr
  , caseExpr, mcaseExpr
  )
where

-- base
import Prelude hiding ( null )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ), litExpr, null )
import Rel8.Kind.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Type ( DBType )


false :: Expr 'NonNullable Bool
false = litExpr False


true :: Expr 'NonNullable Bool
true = litExpr True


(&&.) :: Expr nullability Bool -> Expr nullability Bool -> Expr nullability Bool
Expr a &&. Expr b = Expr (Opaleye.BinExpr Opaleye.OpAnd a b)
infixr 3 &&.


(||.) :: Expr nullability Bool -> Expr nullability Bool -> Expr nullability Bool
Expr a ||. Expr b = Expr (Opaleye.BinExpr Opaleye.OpOr a b)
infixr 2 ||.


not_ :: Expr nullability Bool -> Expr nullability Bool
not_ (Expr a) = Expr (Opaleye.UnExpr Opaleye.OpNot a)


boolExpr :: ()
  => Expr nullability a -> Expr nullability a -> Expr _nullability Bool
  -> Expr nullability a
boolExpr ifFalse ifTrue condition = caseExpr [(condition, ifTrue)] ifFalse


caseExpr :: ()
  => [(Expr _nullability Bool, Expr nullability a)]
  -> Expr nullability a
  -> Expr nullability a
caseExpr branches (Expr fallback) =
  Expr $ Opaleye.CaseExpr (map go branches) fallback
  where
    go (Expr condition, Expr value) = (condition, value)


mcaseExpr :: DBType a
  => [(Expr _nullability Bool, Expr nullability a)]
  -> Expr 'Nullable a
mcaseExpr branches = result
  where
    result = Expr $ Opaleye.CaseExpr (map go branches) fallback
      where
        go (Expr condition, Expr value) = (condition, value)
        Expr fallback = null `asProxyTypeOf` result


asProxyTypeOf :: f a -> proxy a -> f a
asProxyTypeOf = const
