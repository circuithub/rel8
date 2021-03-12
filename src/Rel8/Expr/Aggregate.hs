{-# language DataKinds #-}
{-# language ExistentialQuantification #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Aggregate
  ( Aggregate(..)
  , groupByExpr
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Nullability
  ( Nullability
  , SNullability
  , KnownNullability, nullabilitySing
  )
import Rel8.Type ( TypeInformation(..), typeInformation )
import Rel8.Type.Eq ( DBEq )


type Aggregate :: Nullability -> Type -> Type
data Aggregate nullability a = forall inNullability inType. Aggregate
  { operation :: Maybe (Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct)
  , inNullability :: SNullability inNullability
  , inType :: TypeInformation inType
  , inExpr :: Expr inNullability inType
  , outExpr :: Opaleye.PrimExpr -> Expr nullability a
  }


groupByExpr :: (KnownNullability nullability, DBEq a)
  => Expr nullability a -> Aggregate nullability a
groupByExpr a = Aggregate
  { operation = Nothing
  , inNullability = nullabilitySing
  , inType = typeInformation
  , inExpr = a
  , outExpr = Expr
  }
