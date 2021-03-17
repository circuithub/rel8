{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Num
  ( fromIntegral, realToFrac, div, mod, ceiling, floor, round, truncate
  )
where

-- base
import Prelude ()

-- rel
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Function ( function )
import Rel8.Expr.Opaleye ( castExpr )
import Rel8.Type.DBNum ( DBFractional, DBIntegral, DBNum )


fromIntegral :: (DBIntegral a, DBNum b) => Expr a -> Expr b
fromIntegral (Expr a) = castExpr (Expr a)


realToFrac :: (DBNum a, DBFractional b) => Expr a -> Expr b
realToFrac (Expr a) = castExpr (Expr a)


ceiling :: (DBFractional a, DBIntegral b) => Expr a -> Expr b
ceiling = function "CEILING"


div :: DBIntegral a => Expr a -> Expr a -> Expr a
div = function "DIV"


mod :: DBIntegral a => Expr a -> Expr a -> Expr a
mod = function "MOD"


floor :: (DBFractional a, DBIntegral b) => Expr a -> Expr b
floor = function "FLOOR"


round :: (DBFractional a, DBIntegral b) => Expr a -> Expr b
round = function "ROUND"


truncate :: (DBFractional a, DBIntegral b) => Expr a -> Expr b
truncate = function "TRUNC"

