{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Aggregate where

import Data.Functor
import Data.Monoid
import Data.Profunctor ( dimap, lmap )
import Data.Proxy
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import Rel8.Column
import Rel8.EqTable
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.MonadQuery
import Rel8.Nest
import Rel8.Rewrite
import Rel8.SimpleConstraints
import Rel8.Unconstrained
import Rel8.ZipLeaves


{-| @groupAndAggregate@ is the fundamental aggregation operator in Rel8. Like
SQL, it combines the @GROUP BY@ operator along with aggregation functions to
reduce a sequence of rows to 1 or more rows. If you wish to aggregate any entire
query without grouping, you may find 'aggregate' more convenient.

[ Relationship to @foldMap@ ]

@groupAndAggregate@ is intended to be similar to the 'foldMap' function in
pure Haskell code:

@
foldMap :: Monoid b => ( a -> b ) -> [ a ] -> b
@

@foldMap@ takes a list of values of type @a@, and maps each element to an
element of the monoid @b@. @foldMap@ then combines all of these @b@s together
under their monoid structure to give you a single value.

If we choose @b@ to a map-like structure, we can also use this operation to
do grouping:

@
foldMap :: ( Eq k, Monoid v ) => ( a -> Map k v ) -> [a] -> Map k v
@

It's this instantiation of @foldMap@ that @groupAndAggregate@ attempts to
mirror. If we view @MonadQuery m => m a@ as @[a]@, @groupAndAggregate@ has
a type like:

@
groupAndAggregate
  :: ( EqTable k, MonoidTable v )
  => ( a -> GroupBy k v ) -> [a] -> [(k, v)]
@

This is not quite like @foldMap@, but is more like a @foldMap@ followed by
transforming a @Map k v@ into a list of pairs:

@
\f as -> Map.toList (foldMap f as) :: ( a -> Map k v ) -> [ a ] -> [ ( k, v ) ]
@

-}
groupAndAggregate
  :: forall a k k' v v' m
   . ( MonadQuery m
     , MonoidTable v
     , EqTable k
     , Promote m k' k
     , Promote m v' v
     )
  => ( a -> GroupBy k v ) -> Nest m a -> m ( k', v' )
groupAndAggregate f query =
  aggregate ( eqTableIsImportant . f ) query <&> \GroupBy{ key, value } ->
    ( key, value )

  where

    -- GHC will complain that EqTable isn't necessary. In A sense this is true
    -- as the code doesn't use it at all. However, semantically it's very
    -- important - PostgreSQL will not let us GROUP BY types that can't be
    -- compared for equality.
    eqTableIsImportant :: GroupBy k v -> GroupBy k v
    eqTableIsImportant g@GroupBy{ key } =
      const g ( key ==. key )


-- | Aggregate a table to a single row. This is like @groupAndAggregate@, but
-- where there is only one group.
aggregate
  :: forall a b b' m
   . ( MonadQuery m
     , MonoidTable b
     , Promote m b' b
     )
  => ( a -> b ) -> Nest m a -> m b'
aggregate f =
  liftOpaleye . Opaleye.aggregate ( dimap f to aggregator ) . toOpaleye

  where

    to :: b -> b'
    to =
      rewrite ( \( C x ) -> C ( Expr ( toPrimExpr x ) ) )


class MonoidTable a where
  -- | How to aggregate an entire table.
  aggregator :: Opaleye.Aggregator a a


-- | Higher-kinded records can be used a monoidal aggregations if all fields
-- are instances of 'DBMonoid'.
instance ConstrainHigherKinded m DBMonoid t => MonoidTable ( t ( Expr m ) ) where
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


instance ( ZipLeaves k k ( Expr m ) ( Expr m ), CanZipLeaves k k Unconstrained, MonoidTable v ) => MonoidTable ( GroupBy k v ) where
  aggregator =
    GroupBy
      <$> lmap key group
      <*> lmap value aggregator

    where

      group :: Opaleye.Aggregator k k
      group =
        Opaleye.Aggregator $ Opaleye.PackMap \f a ->
          zipLeaves
            ( Proxy @Unconstrained )
            ( \( C x ) _ -> C . Expr <$> f ( Nothing, toPrimExpr x ) )
            a
            a
