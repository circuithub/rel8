{-# language BlockArguments #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.EqTable ( EqTable, (==.) ) where

import Control.Applicative
import Rel8.Column
import Rel8.DBEq
import Rel8.Expr
import Rel8.HigherKindedTable
import Rel8.Table


-- | The class of database tables (containing one or more columns) that can be
-- compared for equality as a whole.
class EqTable a where
  -- | Compare two tables or expressions for equality.
  --
  -- This operator is overloaded (much like Haskell's 'Eq' type class) to allow
  -- you to compare expressions:
  --
  -- >>> :t exprA
  -- Expr m Int
  --
  -- >>> :t exprA ==. exprA
  -- Expr m Bool
  --
  -- But you can also compare composite structures:
  --
  -- >>> :t ( exprA, exprA ) ==. ( exprA, exprA )
  -- Expr m Bool
  (==.) :: a -> a -> Context a Bool


-- | Any @Expr@s can be compared for equality as long as the underlying
-- database type supports equality comparisons.
instance DBEq a => EqTable ( Expr m a ) where
  (==.) =
    eqExprs


-- | Higher-kinded records can be compared for equality. Two records are equal
-- if all of their fields are equal.
instance ( HigherKindedTable t, HConstrainTable t ( Expr m ) Unconstrained, ConstrainTable ( t ( Expr m ) ) DBEq ) => EqTable ( t ( Expr m ) ) where
  l ==. r =
    and_
      ( getConst
          ( zipTablesWithMC
              @DBEq
              @( t ( Expr m ) )
              ( zipCWithMC @DBEq \a b -> Const [ eqExprs a b ] )
              l
              r
          )
      )
