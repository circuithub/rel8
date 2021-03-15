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
import Rel8.Type.Num ( DBFractional, DBIntegral, DBNum )


fromIntegral :: (DBIntegral a, DBNum b) => Expr nullability a -> Expr nullability b
fromIntegral (Expr a) = castExpr (Expr a)


realToFrac :: (DBNum a, DBFractional b) => Expr nullability a -> Expr nullability b
realToFrac (Expr a) = castExpr (Expr a)


ceiling :: (DBFractional a, DBIntegral b) => Expr nullability a -> Expr nullability b
ceiling = function "CEILING"


div :: DBIntegral a => Expr nullability a -> Expr nullability a -> Expr nullability a
div = function "DIV"


mod :: DBIntegral a => Expr nullability a -> Expr nullability a -> Expr nullability a
mod = function "MOD"


floor :: (DBFractional a, DBIntegral b) => Expr nullability a -> Expr nullability b
floor = function "FLOOR"


round :: (DBFractional a, DBIntegral b) => Expr nullability a -> Expr nullability b
round = function "ROUND"


truncate :: (DBFractional a, DBIntegral b) => Expr nullability a -> Expr nullability b
truncate = function "TRUNC"
