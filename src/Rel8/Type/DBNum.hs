{-# language StandaloneKindSignatures #-}

module Rel8.Type.DBNum ( DBNum(..), DBFractional(..) ) where

import Data.Kind ( Type, Constraint )
import Rel8.Type (DBType)
import {-# SOURCE #-} Rel8.Expr (Expr)


type DBNum :: Type -> Constraint
class DBType a => DBNum a where
  (+.) :: Expr a -> Expr a -> Expr a
  (-.) :: Expr a -> Expr a -> Expr a
  (*.) :: Expr a -> Expr a -> Expr a
  negateExpr :: Expr a -> Expr a
  absExpr :: Expr a -> Expr a
  signumExpr :: Expr a -> Expr a
  fromIntegerExpr :: Integer -> Expr a


type DBFractional :: Type -> Constraint
class DBNum a => DBFractional a where
  fromRationalExpr :: Rational -> Expr a
  recipExpr :: Expr a -> Expr a

