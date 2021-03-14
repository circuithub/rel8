{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Table.Opaleye
  ( aggregator
  , binaryspec
  , distinctspec
  , unpackspec
  , valuesspec
  )
where

-- base
import Data.Functor ( (<&>) )
import Prelude hiding ( undefined )

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye

-- rel8
import Rel8.Expr.Aggregate ( Aggregate( Aggregate ), Aggregator( Aggregator ) )
import qualified Rel8.Expr.Aggregate
import Rel8.Expr.Opaleye ( traversePrimExpr, fromPrimExpr, toPrimExpr )
import Rel8.Schema.Context ( Aggregation( Aggregation ), DB( DB ) )
import Rel8.Schema.HTable ( htabulateA, htraverse, hfield )
import Rel8.Schema.Recontextualize ( Recontextualize )
import Rel8.Table ( Table, Context, fromColumns, toColumns )
import Rel8.Table.Undefined ( undefined )


aggregator :: Recontextualize Aggregation DB aggregates exprs
  => Opaleye.Aggregator aggregates exprs
aggregator = Opaleye.Aggregator $ Opaleye.PackMap $ \f ->
  fmap fromColumns .
  htraverse (\(Aggregation aggregate) -> case aggregate of
    Aggregate {aggregator = maggregator, input, output} ->
      let
        aggregator' = maggregator <&>
          \Aggregator {operation, ordering, distinction} ->
            (operation, ordering, distinction)
      in
        DB . output <$> f (aggregator', input)) .
  toColumns


binaryspec :: (Table a, Context a ~ DB) => Opaleye.Binaryspec a a
binaryspec = Opaleye.Binaryspec $ Opaleye.PackMap $ \f (as, bs) ->
  fmap fromColumns $ htabulateA $ \field ->
    case (hfield (toColumns as) field, hfield (toColumns bs) field) of
      (DB a, DB b) -> DB . fromPrimExpr <$> f (toPrimExpr a, toPrimExpr b)


distinctspec :: (Table a, Context a ~ DB) => Opaleye.Distinctspec a a
distinctspec =
  Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap $ \f ->
    fmap fromColumns .
    htraverse (\(DB a) -> DB . fromPrimExpr <$> f (Nothing, toPrimExpr a)) .
    toColumns


unpackspec :: (Table a, Context a ~ DB) => Opaleye.Unpackspec a a
unpackspec = Opaleye.Unpackspec $ Opaleye.PackMap $ \f ->
  fmap fromColumns .
  htraverse (\(DB a) -> DB <$> traversePrimExpr f a) .
  toColumns


valuesspec :: (Table a, Context a ~ DB) => Opaleye.ValuesspecSafe a a
valuesspec = Opaleye.ValuesspecSafe (toPackmap undefined) unpackspec


toPackmap :: (Table a, Context a ~ DB)
  => a -> Opaleye.PackMap Opaleye.PrimExpr Opaleye.PrimExpr () a
toPackmap as = Opaleye.PackMap $ \f () ->
  fmap fromColumns $
  htraverse (\(DB a) -> DB <$> traversePrimExpr f a) $
  toColumns as
