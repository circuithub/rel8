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
import Rel8.Expr.Null ( isNull, isNonNull, nullable, unsafeLiftOpNullable )
import Rel8.Expr.Opaleye ( zipPrimExprsWith )
import Rel8.Schema.Nullability
  ( Nullability( NonNullable, Nullable )
  , Sql, nullabilization
  )
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
(<.) = case nullabilization @a of
  Nullable -> \ma mb -> isNull ma &&. isNonNull mb ||. ma <? mb
  NonNullable -> lt
infix 4 <.


-- | Corresponds to the SQL @<=@ operator. Note that this differs from SQL @<=@
-- as @null@ will sort below any other value. For a version of @<=@ that exactly
-- matches SQL, see '(<=?)'.
(<=.) :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr Bool
(<=.) = case nullabilization @a of
  Nullable -> \ma mb -> isNull ma ||. ma <=? mb
  NonNullable -> le
infix 4 <=.


-- | Corresponds to the SQL @>@ operator. Note that this differs from SQL @>@
-- as @null@ will sort below any other value. For a version of @>@ that exactly
-- matches SQL, see '(>?)'.
(>.) :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr Bool
(>.) = case nullabilization @a of
  Nullable -> \ma mb -> isNonNull ma &&. isNull mb ||. ma >? mb
  NonNullable -> gt
infix 4 >.


-- | Corresponds to the SQL @>=@ operator. Note that this differs from SQL @>@
-- as @null@ will sort below any other value. For a version of @>=@ that
-- exactly matches SQL, see '(>=?)'.
(>=.) :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr Bool
(>=.) = case nullabilization @a of
  Nullable -> \ma mb -> isNull mb ||. ma >=? mb
  NonNullable -> ge
infix 4 >=.


-- | Corresponds to the SQL @<@ operator. Returns @null@ if either arguments
-- are @null@.
(<?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a <? b = coalesce $ unsafeLiftOpNullable lt a b
infix 4 <?


-- | Corresponds to the SQL @<=@ operator. Returns @null@ if either arguments
-- are @null@.
(<=?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a <=? b = coalesce $ unsafeLiftOpNullable le a b
infix 4 <=?


-- | Corresponds to the SQL @>@ operator. Returns @null@ if either arguments
-- are @null@.
(>?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a >? b = coalesce $ unsafeLiftOpNullable gt a b
infix 4 >?


-- | Corresponds to the SQL @>=@ operator. Returns @null@ if either arguments
-- are @null@.
(>=?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a >=? b = coalesce $ unsafeLiftOpNullable ge a b
infix 4 >=?


-- | Given two expressions, return the expression that sorts less than the
-- other.
-- 
-- Corresponds to the SQL @least()@ function.
leastExpr :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr a
leastExpr ma mb = case nullabilization @a of
  Nullable -> nullable ma (\a -> nullable mb (least_ a) mb) ma
  NonNullable -> least_ ma mb
  where
    least_ (Expr a) (Expr b) = Expr (Opaleye.FunExpr "LEAST" [a, b])


-- | Given two expressions, return the expression that sorts greater than the
-- other.
-- 
-- Corresponds to the SQL @greatest()@ function.
greatestExpr :: forall a. Sql DBOrd a => Expr a -> Expr a -> Expr a
greatestExpr ma mb = case nullabilization @a of
  Nullable -> nullable mb (\a -> nullable ma (greatest_ a) mb) ma
  NonNullable -> greatest_ ma mb
  where
    greatest_ (Expr a) (Expr b) = Expr (Opaleye.FunExpr "GREATEST" [a, b])
