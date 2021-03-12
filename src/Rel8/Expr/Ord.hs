{-# language DataKinds #-}
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
import Rel8.Expr.Bool ( false, true )
import Rel8.Expr.Null ( isNull, isNonNull, nullable )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability, nullabilitySing
  )
import Rel8.Type.Ord ( DBOrd )


(<.) :: forall a nullability. (KnownNullability nullability, DBOrd a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
ma <. mb = case nullabilitySing @nullability of
  SNullable -> nullable (isNonNull mb) (\a -> nullable false (a <?) mb) ma
  SNonNullable -> ma <? mb
infix 4 <.


(<=.) :: forall a nullability. (KnownNullability nullability, DBOrd a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
ma <=. mb = case nullabilitySing @nullability of
  SNullable -> nullable true (\a -> nullable false (a <=?) mb) ma
  SNonNullable -> ma <=? mb
infix 4 <=.


(>.) :: forall a nullability. (KnownNullability nullability, DBOrd a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
ma >. mb = case nullabilitySing @nullability of
  SNullable -> nullable false (\a -> nullable true (a >?) mb) ma
  SNonNullable -> ma >? mb
infix 4 >.


(>=.) :: forall a nullability. (KnownNullability nullability, DBOrd a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
ma >=. mb = case nullabilitySing @nullability of
  SNullable -> nullable (isNull mb) (\a -> nullable true (a >=?) mb) ma
  SNonNullable -> ma >=? mb
infix 4 >=.


(<?) :: DBOrd a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
Expr a <? Expr b = Expr (Opaleye.BinExpr (Opaleye.:<) a b)
infix 4 <?


(<=?) :: DBOrd a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
Expr a <=? Expr b = Expr (Opaleye.BinExpr (Opaleye.:<=) a b)
infix 4 <=?


(>?) :: DBOrd a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
Expr a >? Expr b = Expr (Opaleye.BinExpr (Opaleye.:>) a b)
infix 4 >?


(>=?) :: DBOrd a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
Expr a >=? Expr b = Expr (Opaleye.BinExpr (Opaleye.:>=) a b)
infix 4 >=?


leastExpr :: forall a nullability. (KnownNullability nullability, DBOrd a)
  => Expr nullability a -> Expr nullability a -> Expr nullability a
leastExpr ma mb = case nullabilitySing @nullability of
  SNullable -> nullable ma (\a -> nullable mb (least_ a) mb) ma
  SNonNullable -> least_ ma mb
  where
    least_ (Expr a) (Expr b) = Expr (Opaleye.FunExpr "LEAST" [a, b])


greatestExpr :: forall a nullability. (KnownNullability nullability, DBOrd a)
  => Expr nullability a -> Expr nullability a -> Expr nullability a
greatestExpr ma mb = case nullabilitySing @nullability of
  SNullable -> nullable mb (\a -> nullable ma (greatest_ a) mb) ma
  SNonNullable -> greatest_ ma mb
  where
    greatest_ (Expr a) (Expr b) = Expr (Opaleye.FunExpr "GREATEST" [a, b])
