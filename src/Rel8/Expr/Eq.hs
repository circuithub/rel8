{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Eq
  ( seq, sne
  , (==.), (/=.)
  , (==?), (/=?)
  , isDistinctFrom, isNotDistinctFrom
  )
where

-- base
import Prelude hiding ( seq )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Function ( unsafeBinaryOperator )
import Rel8.Expr.Opaleye ( unsafeZipPrimExprsWith )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability, nullabilitySing
  )
import Rel8.Type.Eq ( DBEq )


seq :: DBEq a
  => SNullability nullability
  -> Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
seq = \case
  SNullable -> isNotDistinctFrom
  SNonNullable -> (==?)


sne :: DBEq a
  => SNullability nullability
  -> Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
sne = \case
  SNullable -> isDistinctFrom
  SNonNullable -> (/=?)


(==.) :: (KnownNullability nullability, DBEq a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
(==.) = seq nullabilitySing
infix 4 ==.


(/=.) :: (KnownNullability nullability, DBEq a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
(/=.) = sne nullabilitySing
infix 4 /=.


(==?) :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
(==?) = unsafeZipPrimExprsWith (Opaleye.BinExpr (Opaleye.:==))
infix 4 ==?


(/=?) :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
(/=?) = unsafeZipPrimExprsWith (Opaleye.BinExpr (Opaleye.:<>))
infix 4 /=?


isDistinctFrom :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
isDistinctFrom = unsafeBinaryOperator "IS DISTINCT FROM"


isNotDistinctFrom :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
isNotDistinctFrom = unsafeBinaryOperator "IS NOT DISTINCT FROM"
