{-# language BlockArguments #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Rel8.Aggregate where

import Data.Monoid
import Data.Profunctor ( dimap, lmap )
import Data.Proxy
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.MonadQuery


foldMap
  :: forall a b m
   . ( MonadQuery m, MonoidTable b )
  => ( a -> b ) -> m a -> m b
foldMap aggregate =
  liftOpaleye . Opaleye.aggregate ( lmap aggregate aggregator ) . toOpaleye


class MonoidTable a where
  aggregator :: Opaleye.Aggregator a a


-- | Higher-kinded records can be used a monoidal aggregations if all fields
-- are instances of 'DBMonoid'.
instance ( ZipRecord t ( Expr m ) ( Expr m ) DBMonoid, HigherKinded t ) => MonoidTable ( t ( Expr m ) ) where
  aggregator =
    Opaleye.Aggregator $ Opaleye.PackMap \f r ->
      zipRecord
        ( Proxy @DBMonoid )
        ( \( C x ) _ -> C <$> Opaleye.runAggregator aggregateExpr f x )
        r
        r


class DBMonoid a where
  aggregateExpr :: Opaleye.Aggregator ( Expr m a ) ( Expr m a )


instance DBMonoid ( Sum a ) where
  aggregateExpr =
    Opaleye.Aggregator $ Opaleye.PackMap \f expr ->
      Expr
        <$> f ( Just ( Opaleye.AggrSum, [], Opaleye.AggrAll )
              , toPrimExpr expr
              )


instance MonoidTable ( Sum ( Expr m a ) ) where
  aggregator =
    dimap from to aggregateExpr

    where

      from :: Sum ( Expr m a ) -> Expr m ( Sum a )
      from ( Sum expr ) =
        Expr ( toPrimExpr expr )


      to :: Expr m ( Sum a ) -> Sum ( Expr m a )
      to expr =
        Sum ( Expr ( toPrimExpr expr ) )
