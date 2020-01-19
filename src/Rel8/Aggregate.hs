{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Aggregate
  ( aggregateExpr
  , aggregator
  , groupAndAggregate
  , aggregate
  , MonoidTable
  , DBMonoid
  , GroupBy(..)
  ) where

import Data.Functor
import Data.Monoid
import Data.Profunctor ( dimap, lmap )
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import Rel8.Column
import Rel8.EqTable
import Rel8.Expr
import Rel8.MonadQuery
import Rel8.Nest
import Rel8.SimpleConstraints
import Rel8.Table
import Rel8.Unconstrained


{-| @groupAndAggregate@ is the fundamental aggregation operator in Rel8. Like
SQL, it combines the @GROUP BY@ operator along with aggregation functions to
reduce a sequence of rows to 1 or more rows. If you wish to aggregate any entire
query without grouping, you may find 'aggregate' more convenient.

=== __Relationship to @foldMap@__

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
  :: ( MonadQuery m
     , MonoidTable v
     , EqTable k
     , Promote m k' k
     , Promote m v' v
     , ConstrainTable v DBMonoid
     , Compatible k ( Expr ( Nest m ) ) k ( Expr ( Nest m ) )
     )
  => ( a -> GroupBy k v ) -> Nest m a -> m ( k', v' )
groupAndAggregate = groupAndAggregate_forAll


groupAndAggregate_forAll
  :: forall a k k' v v' m
   . ( MonadQuery m
     , MonoidTable v
     , EqTable k
     , ConstrainTable v DBMonoid
     , Context k ~ Expr ( Nest m )
     , Context k' ~ Context v'
     , Context v ~ Expr ( Nest m )
     , Context v' ~ Expr m
     , Compatible k' ( Expr m ) k ( Expr ( Nest m ) )
     , Compatible v' ( Expr m ) v ( Expr ( Nest m ) )
     , Compatible k ( Expr ( Nest m ) ) k ( Expr ( Nest m ) )
     )
  => ( a -> GroupBy k v ) -> Nest m a -> m ( k', v' )
groupAndAggregate_forAll f query =
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
  :: ( MonadQuery m
     , MonoidTable b
     , Context b' ~ Expr m
     , Context b ~ Expr ( Nest m)
     , Compatible b' ( Expr m ) b ( Expr ( Nest m ) )
     )
  => ( a -> b ) -> Nest m a -> m b'
aggregate = aggregate_forAll


aggregate_forAll
  :: forall a b b' m
   . ( MonadQuery m
     , MonoidTable b
     , Context b' ~ Expr m
     , Context b ~ Expr ( Nest m )
     , Compatible b' ( Expr m ) b ( Expr ( Nest m ) )
     )
  => ( a -> b ) -> Nest m a -> m b'
aggregate_forAll f =
  liftOpaleye . Opaleye.aggregate ( dimap f to aggregator ) . toOpaleye

  where

    to :: b -> b'
    to =
      mapTable ( \( C x ) -> C ( demote x ) )


-- | The class of tables that can be aggregated. This is like Haskell's 'Monoid'
-- type.
class MonoidTable a where
  -- | How to aggregate an entire table.
  aggregator :: Opaleye.Aggregator a a


-- | Higher-kinded records can be used a monoidal aggregations if all fields
-- are instances of 'DBMonoid'.
instance ( HConstrainTraverse t Unconstrained, ConstrainHigherKinded m DBMonoid t ) => MonoidTable ( t ( Expr m ) ) where
  aggregator =
    Opaleye.Aggregator $ Opaleye.PackMap \f ->
      traverseTableC
        @DBMonoid
        ( \( C x ) -> C <$> Opaleye.runAggregator aggregateExpr f x )


-- | The class of database types that have an aggregation operator.
class DBMonoid a where
  -- | How to aggregate a single expression under a particular monoidal
  -- structure.
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


-- | Group rows of type @v@ by key @k@.
data GroupBy k v =
  GroupBy { key :: k, value :: v }


instance ( Table k, Table v, Context k ~ Context v ) => Table ( GroupBy k v ) where
  type Context ( GroupBy k v ) =
    Context k

  data Field ( GroupBy k v ) x where
    KeyFields :: Field k x -> Field ( GroupBy k v ) x
    ValueFields :: Field v x -> Field ( GroupBy k v ) x

  type ConstrainTable ( GroupBy k v ) c =
    ( ConstrainTable k c, ConstrainTable v c )

  field GroupBy{ key, value } = \case
    KeyFields i -> field key i
    ValueFields i -> field value i

  tabulateMCP proxy f =
    GroupBy
      <$> tabulateMCP proxy ( f . KeyFields )
      <*> tabulateMCP proxy ( f . ValueFields )


instance ( Compatible k f k' g, Compatible v f v' g, Context k ~ Context v, Context k' ~ Context v' ) => Compatible ( GroupBy k v ) f ( GroupBy k' v' ) g where
  transferField = \case
    KeyFields i -> KeyFields ( transferField i )
    ValueFields i -> ValueFields ( transferField i )


instance ( Compatible k ( Expr m ) k ( Expr m ), ConstrainTable k Unconstrained, ConstrainTable v DBMonoid, Context k ~ Expr m, MonoidTable v ) => MonoidTable ( GroupBy k v ) where
  aggregator =
    GroupBy
      <$> lmap key group
      <*> lmap value aggregator

    where

      group :: Opaleye.Aggregator k k
      group =
        Opaleye.Aggregator $ Opaleye.PackMap \f ->
          traverseTable
            ( \( C x ) -> C . Expr <$> f ( Nothing, toPrimExpr x ) )
