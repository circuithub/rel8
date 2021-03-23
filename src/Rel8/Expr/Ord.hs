{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Ord
  ( slt, sle, sgt, sge
  , (<.), (<=.), (>.), (>=.)
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
  , Nullabilizes, nullabilization
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


slt :: DBOrd db => Nullability db a -> Expr a -> Expr a -> Expr Bool
slt = \case
  Nullable -> \ma mb -> isNull ma &&. isNonNull mb ||. ma <? mb
  NonNullable -> lt


sle :: DBOrd db => Nullability db a -> Expr a -> Expr a -> Expr Bool
sle = \case
  Nullable -> \ma mb -> isNull ma ||. ma <=? mb
  NonNullable -> le


sgt :: DBOrd db => Nullability db a -> Expr a -> Expr a -> Expr Bool
sgt = \case
  Nullable -> \ma mb -> isNonNull ma &&. isNull mb ||. ma >? mb
  NonNullable -> gt


sge :: DBOrd db => Nullability db a -> Expr a -> Expr a -> Expr Bool
sge = \case
  Nullable -> \ma mb -> isNull mb ||. ma >=? mb
  NonNullable -> ge


-- | The PostgreSQL @<@ operator.
(<.) :: (DBOrd db, Nullabilizes db a) => Expr a -> Expr a -> Expr Bool
(<.) = slt nullabilization
infix 4 <.


-- | The PostgreSQL @<=@ operator.
(<=.) :: (DBOrd db, Nullabilizes db a) => Expr a -> Expr a -> Expr Bool
(<=.) = sle nullabilization
infix 4 <=.


-- | The PostgreSQL @>@ operator.
(>.) :: (DBOrd db, Nullabilizes db a) => Expr a -> Expr a -> Expr Bool
(>.) = sgt nullabilization
infix 4 >.


-- | The PostgreSQL @>=@ operator.
(>=.) :: (DBOrd db, Nullabilizes db a) => Expr a -> Expr a -> Expr Bool
(>=.) = sge nullabilization
infix 4 >=.


(<?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a <? b = coalesce $ unsafeLiftOpNullable lt a b
infix 4 <?


(<=?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a <=? b = coalesce $ unsafeLiftOpNullable le a b
infix 4 <=?


(>?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a >? b = coalesce $ unsafeLiftOpNullable gt a b
infix 4 >?


(>=?) :: DBOrd a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a >=? b = coalesce $ unsafeLiftOpNullable ge a b
infix 4 >=?


leastExpr :: forall a db. (DBOrd db, Nullabilizes db a)
  => Expr a -> Expr a -> Expr a
leastExpr ma mb = case nullabilization @a of
  Nullable -> nullable ma (\a -> nullable mb (least_ a) mb) ma
  NonNullable -> least_ ma mb
  where
    least_ (Expr a) (Expr b) = Expr (Opaleye.FunExpr "LEAST" [a, b])


greatestExpr :: forall a db. (DBOrd db, Nullabilizes db a)
  => Expr a -> Expr a -> Expr a
greatestExpr ma mb = case nullabilization @a of
  Nullable -> nullable mb (\a -> nullable ma (greatest_ a) mb) ma
  NonNullable -> greatest_ ma mb
  where
    greatest_ (Expr a) (Expr b) = Expr (Opaleye.FunExpr "GREATEST" [a, b])
