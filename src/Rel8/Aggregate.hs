{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Aggregate where

import Data.Monoid
import Data.Profunctor ( dimap, lmap )
import Data.Proxy
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.MonadQuery
import Rel8.Nest
import Rel8.Rewrite
import Rel8.Top
import Rel8.ZipLeaves


aggregateMap
  :: forall a a' b b' m
   . ( MonadQuery m
     , MonoidTable b'
     , Rewrite ( Expr m ) ( Expr ( Nest m ) ) a a'
     , Rewrite ( Expr ( Nest m ) ) ( Expr m ) b' b
     )
  => ( a' -> b' ) -> m a -> m b
aggregateMap aggregate =
  liftOpaleye . Opaleye.aggregate ( dimap from to aggregator ) . toOpaleye

  where

    from :: a -> b'
    from = aggregate . rewrite ( \( C x ) -> C ( Expr ( toPrimExpr x ) ) )

    to :: b' -> b
    to = rewrite ( \( C x ) -> C ( Expr ( toPrimExpr x ) ) )


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


data GroupBy k v =
  GroupBy { key :: k, value :: v }


instance ( Rewrite f g k1 k2, Rewrite f g v1 v2 ) => Rewrite f g ( GroupBy k1 v1 ) ( GroupBy k2 v2 ) where
  rewrite f ( GroupBy k v ) =
    GroupBy ( rewrite f k ) ( rewrite f v )


instance ( ZipLeaves k1 k2 f g, ZipLeaves v1 v2 f g ) => ZipLeaves ( GroupBy k1 v1 ) ( GroupBy k2 v2 ) f g where
  type CanZipLeaves ( GroupBy k1 v1 ) ( GroupBy k2 v2 ) c =
    ( CanZipLeaves k1 k2 c, CanZipLeaves v1 v2 c )

  zipLeaves proxy f a b =
    GroupBy <$> zipLeaves proxy f ( key a ) ( key b ) <*> zipLeaves proxy f ( value a ) ( value b )


instance ( ZipLeaves k k ( Expr m ) ( Expr m ), CanZipLeaves k k Top, MonoidTable v ) => MonoidTable ( GroupBy k v ) where
  aggregator =
    GroupBy
      <$> lmap key group
      <*> lmap value aggregator

    where

      group :: Opaleye.Aggregator k k
      group =
        Opaleye.Aggregator $ Opaleye.PackMap \f a ->
          zipLeaves
            ( Proxy @Top )
            ( \( C x ) _ -> C . Expr <$> f ( Nothing, toPrimExpr x ) )
            a
            a
