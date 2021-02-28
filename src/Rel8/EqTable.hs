{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.EqTable ( EqTable, (==.) ) where

import Rel8


-- | The class of database tables (containing one or more columns) that can be
-- compared for equality as a whole.
class Table Expr a => EqTable a where
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
  (==.) :: a -> a -> Expr Bool


-- | Any @Expr@s can be compared for equality as long as the underlying
-- database type supports equality comparisons.
instance DBEq a => EqTable ( Expr a ) where
  (==.) =
    eqExprs
