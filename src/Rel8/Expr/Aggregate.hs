{-# language DataKinds #-}
{-# language ExistentialQuantification #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Aggregate
  ( Aggregate(..)
  , groupByExpr
  , listAggExpr
  , nonEmptyAggExpr
  , Aggregator(..), unsafeMakeAggregate
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Emptiability ( Emptiability( Emptiable, NonEmptiable ) )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable )
  , SNullability
  , KnownNullability, nullabilitySing
  )
import Rel8.Type ( DBType, TypeInformation(..), typeInformation )
import Rel8.Type.Array ( Array )
import Rel8.Type.Eq ( DBEq )


type Aggregate :: Nullability -> Type -> Type
data Aggregate nullability a = forall inNullability inType. Aggregate
  { aggregator :: Maybe Aggregator
  , inNullability :: SNullability inNullability
  , inType :: TypeInformation inType
  , inExpr :: Expr inNullability inType
  , outExpr :: Opaleye.PrimExpr -> Expr nullability a
  }


groupByExpr :: (KnownNullability nullability, DBEq a)
  => Expr nullability a -> Aggregate nullability a
groupByExpr = unsafeMakeAggregate Nothing


listAggExpr :: (KnownNullability nullability, DBType a)
  => Expr nullability a
  -> Aggregate 'NonNullable (Array 'Emptiable nullability a)
listAggExpr = unsafeMakeAggregate $ Just Aggregator
  { operation = Opaleye.AggrArr
  , ordering = []
  , distinction = Opaleye.AggrAll
  }


nonEmptyAggExpr :: (KnownNullability nullability, DBType a)
  => Expr nullability a
  -> Aggregate 'NonNullable (Array 'NonEmptiable nullability a)
nonEmptyAggExpr = unsafeMakeAggregate $ Just Aggregator
  { operation = Opaleye.AggrArr
  , ordering = []
  , distinction = Opaleye.AggrAll
  }


data Aggregator = Aggregator
  { operation :: Opaleye.AggrOp
  , ordering :: [Opaleye.OrderExpr]
  , distinction :: Opaleye.AggrDistinct
  }


unsafeMakeAggregate :: (KnownNullability inNullability, DBType inType)
  => Maybe Aggregator -> Expr inNullability inType -> Aggregate nullability a
unsafeMakeAggregate aggregator inExpr = Aggregate
  { aggregator
  , inNullability = nullabilitySing
  , inType = typeInformation
  , inExpr
  , outExpr = Expr
  }
