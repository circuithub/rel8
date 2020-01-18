{-# language FlexibleInstances #-}
{-# language KindSignatures #-}

module Rel8.DBEq where

import Data.Int
import Data.Kind
import Data.Text
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Expr


-- | The class of database types that can be compared for equality in queries.
class DBEq ( a :: Type ) where
  eqExprs :: Expr m a -> Expr m a -> Expr m Bool
  eqExprs ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr (Opaleye.:==) a b )


instance DBEq String


instance DBEq Int32


instance DBEq Int64


instance DBEq Text
