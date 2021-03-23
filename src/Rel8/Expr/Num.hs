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
import Rel8.Schema.Nullability ( IsMaybe, Nullabilizes )
import Rel8.Type.Num ( DBFractional, DBIntegral, DBNum )


fromIntegral ::
  ( DBIntegral _a, Nullabilizes _a a
  , DBNum _b, Nullabilizes _b b
  , IsMaybe a ~ IsMaybe b
  )
  => Expr a -> Expr b
fromIntegral (Expr a) = castExpr (Expr a)


realToFrac ::
  ( DBNum _a, Nullabilizes _a a
  , DBFractional _b, Nullabilizes _b b
  , IsMaybe a ~ IsMaybe b
  )
  => Expr a -> Expr b
realToFrac (Expr a) = castExpr (Expr a)


ceiling ::
  ( DBFractional _a, Nullabilizes _a a
  , DBIntegral _b, Nullabilizes _b b
  , IsMaybe a ~ IsMaybe b
  )
  => Expr a -> Expr b
ceiling = function "CEILING"


div :: (DBIntegral _a, Nullabilizes _a a) => Expr a -> Expr a -> Expr a
div = function "DIV"


mod :: (DBIntegral _a, Nullabilizes _a a) => Expr a -> Expr a -> Expr a
mod = function "MOD"


floor ::
  ( DBFractional _a, Nullabilizes _a a
  , DBIntegral _b, Nullabilizes _b b
  , IsMaybe a ~ IsMaybe b
  )
  => Expr a -> Expr b
floor = function "FLOOR"


round ::
  ( DBFractional _a, Nullabilizes _a a
  , DBIntegral _b, Nullabilizes _b b
  , IsMaybe a ~ IsMaybe b
  )
  => Expr a -> Expr b
round = function "ROUND"


truncate ::
  ( DBFractional _a, Nullabilizes _a a
  , DBIntegral _b, Nullabilizes _b b
  , IsMaybe a ~ IsMaybe b
  )
  => Expr a -> Expr b
truncate = function "TRUNC"
