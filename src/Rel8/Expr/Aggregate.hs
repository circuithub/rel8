{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Aggregate
  ( count, countDistinct, countStar, countWhere
  , and, or
  , min, max
  , sum, sumWhere
  , stringAgg
  , groupByExpr
  , listAggExpr, nonEmptyAggExpr
  , slistAggExpr, snonEmptyAggExpr
  )
where

-- base
import Data.Int ( Int64 )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude hiding ( and, max, min, or, sum )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate, Aggregator(..), unsafeMakeAggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( caseExpr, mcaseExpr )
import Rel8.Expr.Opaleye
  ( castExpr
  , litPrimExpr
  , sfromPrimExpr
  , unsafeFromPrimExpr
  , unsafeToPrimExpr
  )
import Rel8.Kind.Blueprint
  ( SBlueprint( SVector )
  , ToDBType
  , blueprintRoundtripsViaDBType
  )
import Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  , SEmptiability( SEmptiable, SNonEmptiable )
  )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable ), SNullability
  , KnownNullability, nullabilitySing
  )
import Rel8.Type ( DBType, blueprintForDBType )
import Rel8.Type.Array ( Array )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Num ( DBNum )
import Rel8.Type.Ord ( DBMax, DBMin )
import Rel8.Type.String ( DBString )
import Rel8.Type.Sum ( DBSum )


count :: Expr nullability a -> Aggregate (Expr 'NonNullable Int64)
count = unsafeMakeAggregate unsafeToPrimExpr unsafeFromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


countDistinct :: DBEq a => Expr nullability a -> Aggregate (Expr 'NonNullable Int64)
countDistinct = unsafeMakeAggregate unsafeToPrimExpr unsafeFromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrCount
    , ordering = []
    , distinction = Opaleye.AggrDistinct
    }


countStar :: Aggregate (Expr 'NonNullable Int64)
countStar = count (litPrimExpr True)


countWhere :: Expr nullability Bool -> Aggregate (Expr 'NonNullable Int64)
countWhere condition = count (mcaseExpr [(condition, litPrimExpr @Int64 0)])


and :: Expr 'NonNullable Bool -> Aggregate (Expr 'NonNullable Bool)
and = unsafeMakeAggregate unsafeToPrimExpr unsafeFromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrBoolAnd
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


or :: Expr 'NonNullable Bool -> Aggregate (Expr 'NonNullable Bool)
or = unsafeMakeAggregate unsafeToPrimExpr unsafeFromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrBoolOr
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


max :: DBMax a => Expr nullability a -> Aggregate (Expr nullability a)
max = unsafeMakeAggregate unsafeToPrimExpr unsafeFromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


min :: DBMin a => Expr nullability a -> Aggregate (Expr nullability a)
min = unsafeMakeAggregate unsafeToPrimExpr unsafeFromPrimExpr $
  Just Aggregator
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


sum :: DBSum a => Expr nullability a -> Aggregate (Expr nullability a)
sum = unsafeMakeAggregate unsafeToPrimExpr (castExpr . unsafeFromPrimExpr) $
  Just Aggregator
    { operation = Opaleye.AggrSum
    , ordering = []
    , distinction = Opaleye.AggrAll
    }


sumWhere :: (DBNum a, DBSum a)
  => Expr nullable Bool -> Expr nullability a -> Aggregate (Expr nullability a)
sumWhere condition a = sum (caseExpr [(condition, a)] 0)


stringAgg :: DBString a
  => Expr 'NonNullable a -> Expr nullability a -> Aggregate (Expr nullability a)
stringAgg delimiter =
  unsafeMakeAggregate unsafeToPrimExpr (castExpr . unsafeFromPrimExpr) $
    Just Aggregator
      { operation = Opaleye.AggrStringAggr (unsafeToPrimExpr delimiter)
      , ordering = []
      , distinction = Opaleye.AggrAll
      }


groupByExpr :: DBEq a => Expr nullability a -> Aggregate (Expr nullability a)
groupByExpr = unsafeMakeAggregate unsafeToPrimExpr unsafeFromPrimExpr Nothing


listAggExpr :: forall a nullability. ()
  => (KnownNullability nullability, DBType a)
  => Expr nullability a
  -> Aggregate (Expr 'NonNullable (Array 'Emptiable nullability a))
listAggExpr = case blueprintForDBType @a of
  blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
    Refl -> slistAggExpr nullabilitySing (blueprintForDBType @a)


nonEmptyAggExpr :: forall a nullability. ()
  => (KnownNullability nullability, DBType a)
  => Expr nullability a
  -> Aggregate (Expr 'NonNullable (Array 'NonEmptiable nullability a))
nonEmptyAggExpr = case blueprintForDBType @a of
  blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
    Refl -> snonEmptyAggExpr nullabilitySing (blueprintForDBType @a)


slistAggExpr :: a ~ ToDBType blueprint
  => SNullability nullability
  -> SBlueprint blueprint
  -> Expr nullability a
  -> Aggregate (Expr 'NonNullable (Array 'Emptiable nullability a))
slistAggExpr nullability blueprint =
  unsafeMakeAggregate unsafeToPrimExpr from $ Just
    Aggregator
      { operation = Opaleye.AggrArr
      , ordering = []
      , distinction = Opaleye.AggrAll
      }
  where
    from = sfromPrimExpr (SVector SEmptiable nullability blueprint)


snonEmptyAggExpr :: a ~ ToDBType blueprint
  => SNullability nullability
  -> SBlueprint blueprint
  -> Expr nullability a
  -> Aggregate (Expr 'NonNullable (Array 'NonEmptiable nullability a))
snonEmptyAggExpr nullability blueprint =
  unsafeMakeAggregate unsafeToPrimExpr from $ Just
    Aggregator
      { operation = Opaleye.AggrArr
      , ordering = []
      , distinction = Opaleye.AggrAll
      }
  where
    from = sfromPrimExpr (SVector SNonEmptiable nullability blueprint)
