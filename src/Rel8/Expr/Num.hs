{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

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
import Rel8.Schema.Nullability ( Homonullable, Sql )
import Rel8.Type.Num ( DBFractional, DBIntegral, DBNum )


fromIntegral :: (Sql DBIntegral a, Sql DBNum b, Homonullable a b)
  => Expr a -> Expr b
fromIntegral (Expr a) = castExpr (Expr a)


realToFrac :: (Sql DBNum a, Sql DBFractional b, Homonullable a b)
  => Expr a -> Expr b
realToFrac (Expr a) = castExpr (Expr a)


ceiling :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
ceiling = function "CEILING"


div :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
div = function "DIV"


mod :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
mod = function "MOD"


floor :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
floor = function "FLOOR"


round :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
round = function "ROUND"


truncate :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
truncate = function "TRUNC"
