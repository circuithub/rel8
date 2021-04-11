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
import Rel8.Schema.Null ( Homonullable, Sql )
import Rel8.Type.Num ( DBFractional, DBIntegral, DBNum )


-- | Cast 'DBIntegral' types to 'DBNum' types. For example, this can be useful
-- if you need to turn an @Expr Int32@ into an @Expr Double@.
fromIntegral :: (Sql DBIntegral a, Sql DBNum b, Homonullable a b)
  => Expr a -> Expr b
fromIntegral (Expr a) = castExpr (Expr a)


-- | Cast 'DBNum' types to 'DBFractional' types. For example, his can be useful
-- to convert @Expr Float@ to @Expr Double@.
realToFrac :: (Sql DBNum a, Sql DBFractional b, Homonullable a b)
  => Expr a -> Expr b
realToFrac (Expr a) = castExpr (Expr a)


-- | Round a 'DBFractional' to a 'DBIntegral' by rounding to the nearest larger
-- integer.
--
-- Corresponds to the @ceiling()@ function.
ceiling :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
ceiling = function "CEILING"


-- | Perform integral division. Corresponds to the @div()@ function.
div :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
div = function "DIV"


-- | Corresponds to the @mod()@ function.
mod :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
mod = function "MOD"


-- | Round a 'DFractional' to a 'DBIntegral' by rounding to the nearest smaller
-- integer. 
--
-- Corresponds to the @floor()@ function.
floor :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
floor = function "FLOOR"


-- | Round a 'DBFractional' to a 'DBIntegral' by rounding to the nearest
-- integer.
--
-- Corresponds to the @round()@ function.
round :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
round = function "ROUND"


-- | Round a 'DBFractional' to a 'DBIntegral' by rounding to the nearest
-- integer towards zero.
truncate :: (Sql DBFractional a, Sql DBIntegral b, Homonullable a b)
  => Expr a -> Expr b
truncate = function "TRUNC"
