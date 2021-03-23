{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Opaleye
  ( aggregator
  , binaryspec
  , distinctspec
  , unpackspec
  , valuesspec
  )
where

-- base
import Prelude hiding ( undefined )

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Internal.Table as Opaleye

-- profunctors
import Data.Profunctor ( dimap, lmap )

-- rel8
import Rel8.Aggregate ( Aggregate( Aggregate ) )
import Rel8.Expr.Opaleye
  ( scastExpr
  , fromPrimExpr, toPrimExpr
  , traversePrimExpr
  , fromColumn, toColumn
  )
import Rel8.Kind.Necessity ( SNecessity( SRequired, SOptional ) )
import Rel8.Schema.Context ( Insertion(..), Name(..) )
import Rel8.Schema.HTable ( htabulateA, hfield, htraverse, hspecs )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Table ( Table, fromColumns, toColumns )
import Rel8.Table.Recontextualize ( Selects )
import Rel8.Table.Undefined ( undefined )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )
import Rel8.Expr (Expr, Col (DB))


aggregator :: Opaleye.Aggregator (Aggregate exprs) exprs
aggregator = Opaleye.Aggregator $ Opaleye.PackMap $
  \f (Aggregate (Opaleye.Aggregator (Opaleye.PackMap inner))) -> inner f ()


binaryspec :: Table Expr a => Opaleye.Binaryspec a a
binaryspec = Opaleye.Binaryspec $ Opaleye.PackMap $ \f (as, bs) ->
  fmap fromColumns $ unwrapApplicative $ htabulateA $ \field ->
    WrapApplicative $
      case (hfield (toColumns as) field, hfield (toColumns bs) field) of
        (DB a, DB b) -> DB . fromPrimExpr <$> f (toPrimExpr a, toPrimExpr b)


distinctspec :: Table Expr a => Opaleye.Distinctspec a a
distinctspec =
  Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap $ \f ->
    fmap fromColumns .
    unwrapApplicative .
    htraverse
      (\(DB a) ->
         WrapApplicative $ DB . fromPrimExpr <$> f (Nothing, toPrimExpr a)) .
    toColumns


unpackspec :: Table Expr a => Opaleye.Unpackspec a a
unpackspec = Opaleye.Unpackspec $ Opaleye.PackMap $ \f ->
  fmap fromColumns .
  unwrapApplicative .
  htraverse (\(DB a) -> WrapApplicative $ DB <$> traversePrimExpr f a) .
  toColumns
{-# INLINABLE unpackspec #-}


valuesspec :: Table Expr a => Opaleye.ValuesspecSafe a a
valuesspec = Opaleye.ValuesspecSafe (toPackMap undefined) unpackspec


toPackMap :: Table Expr a
  => a -> Opaleye.PackMap Opaleye.PrimExpr Opaleye.PrimExpr () a
toPackMap as = Opaleye.PackMap $ \f () ->
  fmap fromColumns $
  unwrapApplicative .
  htraverse (\(DB a) -> WrapApplicative $ DB <$> traversePrimExpr f a) $
  toColumns as
