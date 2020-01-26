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
{-# language UndecidableSuperClasses #-}

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
import Rel8.HigherKindedTable
import Rel8.MonadQuery
import Rel8.Nest
import Rel8.SimpleConstraints
import Rel8.Table


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
     )
  => ( a -> GroupBy k v ) -> Nest m a -> m ( k', v' )
groupAndAggregate = groupAndAggregate_forAll


groupAndAggregate_forAll
  :: forall a k k' v v' m
   . ( MonadQuery m
     , MonoidTable v
     , Recontextualise k Demote
     , Recontextualise v Demote
     , EqTable k
     , Context k ~ Expr ( Nest m )
     , MapTable Demote k ~ k'
     , MapTable Demote v ~ v'
     , Context k' ~ Context v'
     , Context v ~ Expr ( Nest m )
     , Context v' ~ Expr m
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
  :: ( MonadQuery m , MonoidTable b , Promote m b' b )
  => ( a -> b ) -> Nest m a -> m b'
aggregate = aggregate_forAll


aggregate_forAll
  :: forall a b b' m
   . ( MonadQuery m , Promote m b' b , MonoidTable b )
  => ( a -> b ) -> Nest m a -> m b'
aggregate_forAll f =
  liftOpaleye . Opaleye.aggregate ( dimap f to aggregator ) . toOpaleye

  where

    to :: b -> b'
    to =
      mapTable @Demote ( mapC demote )


-- | The class of tables that can be aggregated. This is like Haskell's 'Monoid'
-- type.
class Table a => MonoidTable a where
  -- | How to aggregate an entire table.
  aggregator :: Opaleye.Aggregator a a


-- | Higher-kinded records can be used a monoidal aggregations if all fields
-- are instances of 'DBMonoid'.
instance ( HConstrainTable t ( Expr m ) Unconstrained, HigherKindedTable t, ConstrainTable ( t ( Expr m ) ) DBMonoid ) => MonoidTable ( t ( Expr m ) ) where
  aggregator =
    Opaleye.Aggregator $ Opaleye.PackMap \f ->
      traverseTableC
        @Id
        @DBMonoid
        ( traverseCC @DBMonoid ( Opaleye.runAggregator aggregateExpr f ) )


-- | The class of database types that have an aggregation operator.
class DBMonoid a where
  -- | How to aggregate a single expression under a particular monoidal
  -- structure.
  aggregateExpr :: Opaleye.Aggregator ( Expr m a ) ( Expr m a )


instance DBMonoid ( Sum a ) where
  aggregateExpr =
    Opaleye.Aggregator $ Opaleye.PackMap \f expr ->
      fromPrimExpr
        <$> f ( Just ( Opaleye.AggrSum, [], Opaleye.AggrAll )
              , toPrimExpr expr
              )


instance MonoidTable ( Sum ( Expr m a ) ) where
  aggregator =
    dimap from to aggregateExpr

    where

      from :: Sum ( Expr m a ) -> Expr m ( Sum a )
      from ( Sum expr ) =
        retype expr


      to :: Expr m ( Sum a ) -> Sum ( Expr m a )
      to expr =
        Sum ( retype expr )


-- | Group rows of type @v@ by key @k@.
data GroupBy k v =
  GroupBy { key :: k, value :: v }


data GroupByField k v a where
  KeyFields :: Field k x -> GroupByField k v x
  ValueFields :: Field v x -> GroupByField k v x


instance ( Table k, Table v, Context k ~ Context v ) => Table ( GroupBy k v ) where
  type Context ( GroupBy k v ) =
    Context k

  type Field ( GroupBy k v ) =
    GroupByField k v

  type ConstrainTable ( GroupBy k v ) c =
    ( ConstrainTable k c, ConstrainTable v c )

  field GroupBy{ key, value } = \case
    KeyFields i -> field key i
    ValueFields i -> field value i

  tabulateMCP proxy f =
    GroupBy
      <$> tabulateMCP proxy ( f . KeyFields )
      <*> tabulateMCP proxy ( f . ValueFields )


instance ( Context k ~ Context v, Context ( MapTable f k ) ~ Context ( MapTable f v ), Recontextualise k f, Recontextualise v f ) => Recontextualise ( GroupBy k v ) f where
  type MapTable f ( GroupBy k v ) =
    GroupBy ( MapTable f k ) ( MapTable f v )

  fieldMapping = \case
    KeyFields i -> KeyFields ( fieldMapping @_ @f i )
    ValueFields i -> ValueFields ( fieldMapping @_ @f i )

  reverseFieldMapping = \case
    KeyFields i -> KeyFields ( reverseFieldMapping @_ @f i )
    ValueFields i -> ValueFields ( reverseFieldMapping @_ @f i )


instance ( Recontextualise k Id, Context v ~ Expr m, Table k, Context k ~ Expr m, MonoidTable v ) => MonoidTable ( GroupBy k v ) where
  aggregator =
    GroupBy
      <$> lmap key group
      <*> lmap value aggregator

    where

      group :: Opaleye.Aggregator k k
      group =
        Opaleye.Aggregator $ Opaleye.PackMap \f ->
          traverseTable @Id ( traverseC \x -> fromPrimExpr <$> f ( Nothing, toPrimExpr x ) )
