{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
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
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Type.Array ( Array )
import Rel8.Type.Eq ( DBEq )


type Aggregate :: Nullability -> Type -> Type
data Aggregate nullability a = Aggregate
  { aggregator :: Maybe Aggregator
  , input :: Opaleye.PrimExpr
  , output :: Opaleye.PrimExpr -> Expr nullability a
  }


data Aggregator = Aggregator
  { operation :: Opaleye.AggrOp
  , ordering :: [Opaleye.OrderExpr]
  , distinction :: Opaleye.AggrDistinct
  }


groupByExpr :: DBEq a => Expr nullability a -> Aggregate nullability a
groupByExpr = unsafeMakeAggregate Nothing


listAggExpr :: ()
  => Expr nullability a
  -> Aggregate 'NonNullable (Array 'Emptiable nullability a)
listAggExpr = unsafeMakeAggregate $ Just Aggregator
  { operation = Opaleye.AggrArr
  , ordering = []
  , distinction = Opaleye.AggrAll
  }


nonEmptyAggExpr :: ()
  => Expr nullability a
  -> Aggregate 'NonNullable (Array 'NonEmptiable nullability a)
nonEmptyAggExpr = unsafeMakeAggregate $ Just Aggregator
  { operation = Opaleye.AggrArr
  , ordering = []
  , distinction = Opaleye.AggrAll
  }


unsafeMakeAggregate :: ()
  => Maybe Aggregator -> Expr _nullability _a -> Aggregate nullability a
unsafeMakeAggregate aggregator (Expr input) = Aggregate
  { aggregator
  , input
  , output = Expr
  }
