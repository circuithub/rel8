{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Ord
  ( (<.), (<=.), (>.), (>=.)
  , (<?), (<=?), (>?), (>=?)
  , leastExpr, greatestExpr
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Bool ( (&&.), (||.), coalesce )
import Rel8.Expr.Null ( isNull, isNonNull, nullable, unsafeLiftOpNull )
import Rel8.Expr.Opaleye ( toPrimExpr, zipPrimExprsWith )
import Rel8.Schema.Null ( Nullity( Null, NotNull ), Sql )
import qualified Rel8.Schema.Null as Schema ( nullable )
import Rel8.Type.Ord ( DBOrd )


lt :: DBOrd a => Expr a -> Expr a -> Expr Bool
lt = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:<))


le :: DBOrd a => Expr a -> Expr a -> Expr Bool
le = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:<=))


gt :: DBOrd a => Expr a -> Expr a -> Expr Bool
gt = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:>))


ge :: DBOrd a => Expr a -> Expr a -> Expr Bool
ge = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:>=))


-- | Corresponds to the SQL @<@ operator. Note that this differs from SQL @<@
-- as @null@ will sort below any other value. For a version of @<@ that exactly
-- matches SQL, see '(<?)'.
(<.) :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr Bool
(<.) = case Schema.nullable @a of
  Null -> \ma mb -> isNull ma &&. isNonNull mb ||. ma <? mb
  NotNull -> lt
infix 4 <.


-- | Corresponds to the SQL @<=@ operator. Note that this differs from SQL @<=@
-- as @null@ will sort below any other value. For a version of @<=@ that exactly
-- matches SQL, see '(<=?)'.
(<=.) :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr Bool
(<=.) = case Schema.nullable @a of
  Null -> \ma mb -> isNull ma ||. ma <=? mb
  NotNull -> le
infix 4 <=.


-- | Corresponds to the SQL @>@ operator. Note that this differs from SQL @>@
-- as @null@ will sort below any other value. For a version of @>@ that exactly
-- matches SQL, see '(>?)'.
(>.) :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr Bool
(>.) = case Schema.nullable @a of
  Null -> \ma mb -> isNonNull ma &&. isNull mb ||. ma >? mb
  NotNull -> gt
infix 4 >.


-- | Corresponds to the SQL @>=@ operator. Note that this differs from SQL @>@
-- as @null@ will sort below any other value. For a version of @>=@ that
-- exactly matches SQL, see '(>=?)'.
(>=.) :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr Bool
(>=.) = case Schema.nullable @a of
  Null -> \ma mb -> isNull mb ||. ma >=? mb
  NotNull -> ge
infix 4 >=.


-- | Corresponds to the SQL @<@ operator. Returns @null@ if either arguments
-- are @null@.
(<?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a <? b = coalesce $ unsafeLiftOpNull lt a b
infix 4 <?


-- | Corresponds to the SQL @<=@ operator. Returns @null@ if either arguments
-- are @null@.
(<=?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a <=? b = coalesce $ unsafeLiftOpNull le a b
infix 4 <=?


-- | Corresponds to the SQL @>@ operator. Returns @null@ if either arguments
-- are @null@.
(>?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a >? b = coalesce $ unsafeLiftOpNull gt a b
infix 4 >?


-- | Corresponds to the SQL @>=@ operator. Returns @null@ if either arguments
-- are @null@.
(>=?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a >=? b = coalesce $ unsafeLiftOpNull ge a b
infix 4 >=?


-- | Given two expressions, return the expression that sorts less than the
-- other.
-- 
-- Corresponds to the SQL @least()@ function.
leastExpr :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr a
leastExpr ma mb = case Schema.nullable @a of
  Null -> nullable ma (\a -> nullable mb (least_ a) mb) ma
  NotNull -> least_ ma mb
  where
    least_ a b = Expr (Opaleye.FunExpr "LEAST" [toPrimExpr a, toPrimExpr b])


-- | Given two expressions, return the expression that sorts greater than the
-- other.
-- 
-- Corresponds to the SQL @greatest()@ function.
greatestExpr :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr a
greatestExpr ma mb = case Schema.nullable @a of
  Null -> nullable mb (\a -> nullable ma (greatest_ a) mb) ma
  NotNull -> greatest_ ma mb
  where
    greatest_ a b =
      Expr (Opaleye.FunExpr "GREATEST" [toPrimExpr a, toPrimExpr b])
