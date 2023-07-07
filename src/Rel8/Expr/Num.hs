{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Rel8.Expr.Num (
  fromIntegral,
  realToFrac,
  div,
  mod,
  divMod,
  quot,
  rem,
  quotRem,
  ceiling,
  floor,
  round,
  truncate,
)
where

-- base
import Prelude (fst, negate, signum, snd, (+), (-))

-- rel8
import Rel8.Expr (Expr (Expr))
import Rel8.Expr.Eq ((==.))
import Rel8.Expr.Function (function)
import Rel8.Expr.Opaleye (castExpr)
import Rel8.Schema.Null (Homonullable, Sql)
import Rel8.Table.Bool (bool)
import Rel8.Type.Num (DBFractional, DBIntegral, DBNum)


{- | Cast 'DBIntegral' types to 'DBNum' types. For example, this can be useful
if you need to turn an @Expr Int32@ into an @Expr Double@.
-}
fromIntegral ::
  (Sql DBIntegral a, Sql DBNum b, Homonullable a b) =>
  Expr a ->
  Expr b
fromIntegral (Expr a) = castExpr (Expr a)


{- | Cast 'DBNum' types to 'DBFractional' types. For example, his can be useful
to convert @Expr Float@ to @Expr Double@.
-}
realToFrac ::
  (Sql DBNum a, Sql DBFractional b, Homonullable a b) =>
  Expr a ->
  Expr b
realToFrac (Expr a) = castExpr (Expr a)


{- | Round a 'DBFractional' to a 'DBIntegral' by rounding to the nearest larger
integer.

Corresponds to the @ceiling()@ function.
-}
ceiling ::
  (Sql DBFractional a, Sql DBIntegral b, Homonullable a b) =>
  Expr a ->
  Expr b
ceiling = function "ceiling"


{- | Emulates the behaviour of the Haskell function 'Prelude.div' in
PostgreSQL.
-}
div :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
div n d = fst (divMod n d)


{- | Emulates the behaviour of the Haskell function 'Prelude.mod' in
PostgreSQL.
-}
mod :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
mod n d = snd (divMod n d)


-- | Simultaneous 'div' and 'mod'.
divMod :: Sql DBIntegral a => Expr a -> Expr a -> (Expr a, Expr a)
divMod n d = bool qr (q - 1, r + d) (signum r ==. negate (signum d))
  where
    qr@(q, r) = quotRem n d


{- | Perform integral division. Corresponds to the @div()@ function in
PostgreSQL, which behaves like Haskell's 'Prelude.quot' rather than
'Prelude.div'.
-}
quot :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
quot = function "div"


{- | Corresponds to the @mod()@ function in PostgreSQL, which behaves like
Haskell's 'Prelude.rem' rather than 'Prelude.mod'.
-}
rem :: Sql DBIntegral a => Expr a -> Expr a -> Expr a
rem = function "mod"


-- | Simultaneous 'quot' and 'rem'.
quotRem :: Sql DBIntegral a => Expr a -> Expr a -> (Expr a, Expr a)
quotRem n d = (quot n d, rem n d)


{- | Round a 'DFractional' to a 'DBIntegral' by rounding to the nearest smaller
integer.

Corresponds to the @floor()@ function.
-}
floor ::
  (Sql DBFractional a, Sql DBIntegral b, Homonullable a b) =>
  Expr a ->
  Expr b
floor = function "floor"


{- | Round a 'DBFractional' to a 'DBIntegral' by rounding to the nearest
integer.

Corresponds to the @round()@ function.
-}
round ::
  (Sql DBFractional a, Sql DBIntegral b, Homonullable a b) =>
  Expr a ->
  Expr b
round = function "round"


{- | Round a 'DBFractional' to a 'DBIntegral' by rounding to the nearest
integer towards zero.
-}
truncate ::
  (Sql DBFractional a, Sql DBIntegral b, Homonullable a b) =>
  Expr a ->
  Expr b
truncate = function "trunc"
