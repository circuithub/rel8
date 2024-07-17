{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Aggregate
  ( Aggregator' (Aggregator)
  , Aggregator
  , Aggregator1
  , toAggregator
  , toAggregator1
  , filterWhereExplicit
  , unsafeMakeAggregator
  )
where

-- base
import Control.Applicative (liftA2)
import Data.Kind (Type)
import Prelude

-- opaleye
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.MaybeFields as Opaleye
import qualified Opaleye.Internal.Operators as Opaleye

-- product-profunctor
import Data.Profunctor.Product
  ( ProductProfunctor, purePP, (****)
  , SumProfunctor, (+++!)
  )

-- profunctors
import Data.Profunctor (Profunctor, dimap)

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (toPrimExpr, toColumn)
import Rel8.Aggregate.Fold (Fallback (Empty, Fallback), Fold (Full, Semi))

-- semigroupoids
import Data.Functor.Apply (Apply, liftF2)


-- | 'Aggregator'' is the most general form of \"aggregator\", of which
-- 'Aggregator' and 'Aggregator1' are special cases. 'Aggregator''s are
-- comprised of aggregation functions and/or @GROUP BY@ clauses.
--
-- Aggregation functions operating on individual 'Rel8.Expr's such as
-- 'Rel8.sum' can be combined into 'Aggregator's operating on larger types
-- using the 'Applicative', 'Profunctor' and 'ProductProfunctor' interfaces.
-- Working with 'Profunctor's can sometimes be awkward so for every 'Rel8.sum'
-- we also provide a 'Rel8.sumOn' which bundles an 'Data.Profunctor.lmap'. For
-- complex aggregations, we recommend using these functions along with
-- @ApplicativeDo@, @BlockArguments@, @OverloadedRecordDot@ and
-- @RecordWildCards@:
--
-- @
--
-- data Input f = Input
--   { orderId :: Column f OrderId
--   , customerId :: Column f CustomerId
--   , productId :: Column f ProductId
--   , quantity :: Column f Int64
--   , price :: Column f Scientific
--   }
--   deriving (Generic, Rel8able)
--
--
-- totalPrice :: Input Expr -> Expr Scientific
-- totalPrice input = fromIntegral input.quantity * input.price
--
--
-- data Result f = Result
--   { customerId :: Column f CustomerId
--   , totalOrders :: Column f Int64
--   , productsOrdered :: Column f Int64
--   , totalPrice :: Column Scientific
--   }
--   deriving (Generic, Rel8able)
--
--
-- allResults :: Query (Result Expr)
-- allResults =
--   aggregate
--     do
--       customerId <- groupByOn (.customerId)
--       totalOrders <- countDistinctOn (.orderId)
--       productsOrdered <- countDistinctOn (.productId)
--       totalPrice <- sumOn totalPrice
--       pure Result {..}
--     do
--       order <- each orderSchema
--       orderLine <- each orderLineSchema
--       where_ $ order.id ==. orderLine.orderId
--       pure
--         Input
--           { orderId = order.id
--           , customerId = order.customerId
--           , productId = orderLine.productId
--           , quantity = orderLine.quantity
--           , price = orderLine.price
--           }
-- @
type Aggregator' :: Fold -> Type -> Type -> Type
data Aggregator' fold i a = Aggregator !(Fallback fold a) !(Opaleye.Aggregator i a)


instance Profunctor (Aggregator' fold) where
  dimap f g (Aggregator fallback a) =
    Aggregator (fmap g fallback) (dimap f g a)


instance ProductProfunctor (Aggregator' fold) where
  purePP = pure
  (****) = (<*>)


instance SumProfunctor (Aggregator' fold) where
  Aggregator fallback a +++! Aggregator fallback' b =
    flip Aggregator (a +++! b) $ case fallback of
      Empty -> case fallback' of
        Empty -> Empty
        Fallback x -> Fallback (Right x)
      Fallback x -> Fallback (Left x)


instance Functor (Aggregator' fold i) where
  fmap = dimap id


instance Apply (Aggregator' fold i) where
  liftF2 f (Aggregator fallback a) (Aggregator fallback' b) =
    Aggregator (liftF2 f fallback fallback') (liftA2 f a b)


instance Applicative (Aggregator' fold i) where
  pure a = Aggregator (pure a) (pure a)
  liftA2 = liftF2


-- | An 'Aggregator' takes a 'Rel8.Query' producing a collection of rows of
-- type @a@ and transforms it into a 'Rel8.Query' producing a single row of
-- type @b@. If the given 'Rel8.Query' produces an empty collection of rows,
-- then the single row in the resulting 'Rel8.Query' contains the identity
-- values of the aggregation functions comprising the 'Aggregator' (i.e.,
-- @0@ for 'Rel8.sum', 'Rel8.false' for 'Rel8.or', etc.).
--
-- 'Aggregator' is a special form of 'Aggregator'' parameterised by 'Full'.
type Aggregator :: Type -> Type -> Type
type Aggregator = Aggregator' 'Full


-- | An 'Aggregator1' takes a collection of rows of type @a@, groups them, and
-- transforms each group into a single row of type @b@. This corresponds to
-- aggregators using @GROUP BY@ in SQL. If given an empty collection of rows,
-- 'Aggregator1' will have no groups and will therefore also return an empty
-- collection of rows.
--
-- 'Aggregator1' is a special form of 'Aggregator'' parameterised by 'Semi'.
type Aggregator1 :: Type -> Type -> Type
type Aggregator1 = Aggregator' 'Semi


-- | 'toAggregator1' turns an 'Aggregator' into an 'Aggregator1'.
toAggregator1 :: Aggregator' fold i a -> Aggregator1 i a
toAggregator1 (Aggregator _ a) = Aggregator Empty a


-- | Given a value to fall back on if given an empty collection of rows,
-- 'toAggregator' turns an 'Aggregator1' into an 'Aggregator'.
toAggregator :: a -> Aggregator' fold i a -> Aggregator' fold' i a
toAggregator fallback (Aggregator _ a) = Aggregator (Fallback fallback) a


filterWhereExplicit :: ()
  => Opaleye.IfPP a a
  -> (i -> Expr Bool)
  -> Aggregator i a
  -> Aggregator' fold i a
filterWhereExplicit ifPP f (Aggregator (Fallback fallback) aggregator) =
  Aggregator (Fallback fallback) aggregator'
  where
    aggregator' =
      Opaleye.fromMaybeFieldsExplicit ifPP fallback
        <$> Opaleye.filterWhere (toColumn . toPrimExpr . f) aggregator


unsafeMakeAggregator :: forall (i :: Type) (o :: Type) (fold :: Fold) i' o'.  ()
  => (i -> i')
  -> (o' -> o)
  -> Fallback fold o
  -> Opaleye.Aggregator i' o'
  -> Aggregator' fold i o
unsafeMakeAggregator input output fallback =
  Aggregator fallback . dimap input output
