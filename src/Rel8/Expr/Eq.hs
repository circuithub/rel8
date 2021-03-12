{-# language DataKinds #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Eq
  ( (==.), (/=.)
  , (==?), (/=?)
  , isDistinctFrom, isNotDistinctFrom
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability, nullabilitySing
  )
import Rel8.Type.Eq ( DBEq )


(==.) :: forall a nullability. (KnownNullability nullability, DBEq a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
(==.) = case nullabilitySing @nullability of
  SNullable -> isNotDistinctFrom
  SNonNullable -> (==?)
infix 4 ==.


(/=.) :: forall a nullability. (KnownNullability nullability, DBEq a)
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
(/=.) = case nullabilitySing @nullability of
  SNullable -> isDistinctFrom
  SNonNullable -> (/=?)
infix 4 /=.


(==?) :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
Expr a ==? Expr b = Expr (Opaleye.BinExpr (Opaleye.:==) a b)
infix 4 ==?


(/=?) :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr nullability Bool
Expr a /=? Expr b = Expr (Opaleye.BinExpr (Opaleye.:<>) a b)
infix 4 /=?


isDistinctFrom :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
isDistinctFrom (Expr a) (Expr b) =
  Expr (Opaleye.BinExpr (Opaleye.OpOther "IS DISTINCT FROM") a b)


isNotDistinctFrom :: DBEq a
  => Expr nullability a -> Expr nullability a -> Expr 'NonNullable Bool
isNotDistinctFrom (Expr a) (Expr b) =
  Expr (Opaleye.BinExpr (Opaleye.OpOther "IS NOT DISTINCT FROM") a b)
