{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Aggregate
  ( Aggregate(..)
  , count, countDistinct, countStar, countWhere
  , and, or
  , min, max
  , sum, sumWhere
  , stringAgg
  , groupByExpr
  , listAggExpr, nonEmptyAggExpr
  , Aggregator(..)
  , unsafeMakeAggregate
  )
where

-- base
import Data.Int ( Int64 )
import Data.Kind ( Type )
import Prelude hiding ( and, max, min, or, sum )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( caseExpr, mcaseExpr )
import Rel8.Expr.Opaleye
  ( castExpr
  , litPrimExpr
  , fromPrimExpr
  , unsafeFromPrimExpr
  , unsafeToPrimExpr
  )
import Rel8.Kind.Emptiability ( Emptiability( Emptiable, NonEmptiable ) )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Type.Array ( Array )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Num ( DBNum )
import Rel8.Type.Ord ( DBMax, DBMin )
import Rel8.Type.String ( DBString )
import Rel8.Type.Sum ( DBSum )


count :: Expr nullability a -> Aggregate 'NonNullable Int64
count = unsafeMakeAggregate unsafeFromPrimExpr unsafeToPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


countDistinct :: DBEq a => Expr nullability a -> Aggregate 'NonNullable Int64
countDistinct = unsafeMakeAggregate unsafeFromPrimExpr unsafeToPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrDistinct
    }


countStar :: Aggregate 'NonNullable Int64
countStar = count (litPrimExpr True)


countWhere :: Expr nullability Bool -> Aggregate 'NonNullable Int64
countWhere condition = count (mcaseExpr [(condition, litPrimExpr @Int64 0)])


and :: Expr 'NonNullable Bool -> Aggregate 'NonNullable Bool
and = unsafeMakeAggregate unsafeFromPrimExpr unsafeToPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrBoolAnd
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


or :: Expr 'NonNullable Bool -> Aggregate 'NonNullable Bool
or = unsafeMakeAggregate unsafeFromPrimExpr unsafeToPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrBoolOr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


max :: DBMax a => Expr nullability a -> Aggregate nullability a
max = unsafeMakeAggregate unsafeFromPrimExpr unsafeToPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


min :: DBMin a => Expr nullability a -> Aggregate nullability a
min = unsafeMakeAggregate unsafeFromPrimExpr unsafeToPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


sum :: DBSum a => Expr nullability a -> Aggregate nullability a
sum = unsafeMakeAggregate (castExpr . unsafeFromPrimExpr) unsafeToPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


sumWhere :: (DBNum a, DBSum a)
  => Expr nullable Bool -> Expr nullability a -> Aggregate nullability a
sumWhere condition a = sum (caseExpr [(condition, a)] 0)


stringAgg :: DBString a
  => Expr 'NonNullable a -> Expr nullability a -> Aggregate nullability a
stringAgg delimiter =
  unsafeMakeAggregate (castExpr . unsafeFromPrimExpr) unsafeToPrimExpr $
    Just Aggregator
      { operation = Opaleye.AggrStringAggr (unsafeToPrimExpr delimiter)
      , ordering = []
      , distinction = Opaleye.AggrAll
      }


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
groupByExpr = unsafeMakeAggregate unsafeFromPrimExpr unsafeToPrimExpr Nothing


listAggExpr :: ()
  => Expr nullability a
  -> Aggregate 'NonNullable (Array 'Emptiable nullability a)
listAggExpr = unsafeMakeAggregate fromPrimExpr unsafeToPrimExpr $ Just
  Aggregator
    { operation = Opaleye.AggrArr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


nonEmptyAggExpr :: ()
  => Expr nullability a
  -> Aggregate 'NonNullable (Array 'NonEmptiable nullability a)
nonEmptyAggExpr = unsafeMakeAggregate fromPrimExpr unsafeToPrimExpr $ Just
  Aggregator
    { operation = Opaleye.AggrArr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


unsafeMakeAggregate :: ()
  => (Opaleye.PrimExpr -> Expr nullability a)
  -> (Expr _nullability _a -> Opaleye.PrimExpr)
  -> Maybe Aggregator
  -> Expr _nullability _a
  -> Aggregate nullability a
unsafeMakeAggregate output prime aggregator input =
  Aggregate
    { aggregator
    , input = prime input
    , output
    }
