{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Eq
  ( seq, sne
  , (==.), (/=.)
  , (==?), (/=?)
  , in_
  , isDistinctFrom, isNotDistinctFrom
  )
where

-- base
import Data.Foldable ( toList )
import Prelude hiding ( seq, sin )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Array ( listOf )
import Rel8.Expr.Bool ( false, or_ )
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


sin :: (DBEq a, Foldable f)
  => SNullability nullability
  -> f (Expr nullability a) -> Expr nullability a -> Expr 'NonNullable Bool
sin nullability (toList -> as) a = case nullability of
  SNullable -> or_ $ map (`isNotDistinctFrom` a) as
  SNonNullable -> case as of
     [] -> false
     _ -> unsafeZipPrimExprsWith (Opaleye.BinExpr Opaleye.OpIn) a (listOf as)


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


in_ :: (KnownNullability nullability, DBEq a, Foldable f)
  => f (Expr nullability a) -> Expr nullability a -> Expr 'NonNullable Bool
in_ = sin nullabilitySing


isDistinctFrom :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
isDistinctFrom = unsafeBinaryOperator "IS DISTINCT FROM"


isNotDistinctFrom :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
isNotDistinctFrom = unsafeBinaryOperator "IS NOT DISTINCT FROM"
