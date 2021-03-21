{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# language TypeApplications #-}

{-# options -Wno-orphans #-}

module Rel8.Expr.Instances ( Column( ExprColumn, fromExprColumn ) ) where

import Rel8.Context ( Context( Column ), Meta( Meta ) )
import Rel8.Expr ( Expr )
import Data.Kind (Type)
import Rel8.Info (HasInfo)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Data.String ( IsString( fromString ) )
import qualified Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import Rel8.Expr.Opaleye (columnToExpr, exprToColumn, litExpr)
import Rel8.Expr.Function (function)


instance Context Expr where
  data Column Expr :: Meta -> Type where
    ExprColumn :: { fromExprColumn :: Expr a } -> Column Expr ('Meta defaulting a)


-- | It is assumed that any Haskell types that have a 'Num' instance also have
-- the corresponding operations in the database. Hence, Num a => Num (Expr a).
-- *However*, if this is not the case, you should `newtype` the Haskell type
-- and avoid providing a 'Num' instance, or you may write be able to write
-- ill-typed queries!
instance (HasInfo a, Num a) => Num (Expr a) where
  a + b = columnToExpr (Opaleye.binOp (Opaleye.:+) (exprToColumn a) (exprToColumn b))
  a - b = columnToExpr (Opaleye.binOp (Opaleye.:-) (exprToColumn a) (exprToColumn b))
  a * b = columnToExpr (Opaleye.binOp (Opaleye.:*) (exprToColumn a) (exprToColumn b))
  abs = function "abs"
  signum = columnToExpr @Opaleye.PGInt8 . signum . exprToColumn
  fromInteger = litExpr . fromInteger
  negate = columnToExpr @Opaleye.PGInt8 . negate . exprToColumn


instance (HasInfo a, Fractional a) => Fractional (Expr a) where
  a / b = columnToExpr (Opaleye.binOp (Opaleye.:/) (exprToColumn a) (exprToColumn b))
  fromRational = litExpr . fromRational


instance (IsString a, HasInfo a) => IsString (Expr a) where
  fromString = litExpr . fromString
