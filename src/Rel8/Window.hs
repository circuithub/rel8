{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Window
  ( Window(..)
  , Partition
  , over
  , partitionBy
  , orderPartitionBy
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Contravariant ( Contravariant, contramap )
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Window as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- profunctors
import Data.Profunctor ( Profunctor )

-- product-profunctors
import Data.Profunctor.Product ( ProductProfunctor, (****), purePP )

-- rel8
import Rel8.Expr.Opaleye ( toColumn, toPrimExpr )
import Rel8.Order( Order( Order ) )
import Rel8.Schema.HTable ( hfield, htabulateA )
import Rel8.Table ( Columns, toColumns )
import Rel8.Table.Eq ( EqTable )

-- semigroupoids
import Data.Functor.Apply ( Apply, WrappedApplicative(..) )


-- | 'Window' is an applicative functor that represents expressions that
-- contain
-- [window functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- 'Rel8.Query.Window.window' can be used to
-- evaluate these expressions over a particular query.
type Window :: Type -> Type -> Type
newtype Window a b = Window (Opaleye.Windows a b)
  deriving newtype (Profunctor)
  deriving newtype (Functor, Applicative)
  deriving (Apply) via (WrappedApplicative (Window a))


instance ProductProfunctor Window where
  purePP = pure
  (****) = (<*>)


-- | In PostgreSQL, window functions must specify the \"window\" or
-- \"partition\" over which they operate. The syntax for this looks like:
-- @SUM(salary) OVER (PARTITION BY department)@. The Rel8 type 'Partition'
-- represents everything that comes after @OVER@.
--
-- 'Partition' is a 'Monoid', so 'Window's created with 'partitionBy' and
-- 'orderWindowBy' can be combined using '<>'.
type Partition :: Type -> Type
newtype Partition a = Partition (Opaleye.Window a)
  deriving newtype (Contravariant, Semigroup, Monoid)


-- | 'over' adds a 'Partition' to a 'Window' expression.
--
-- @@@
-- 'Rel8.Table.Window.cumulative' ('Rel8.Expr.Aggregate.sum' . salary) `over` 'partitionBy' department <> 'orderPartitionBy' (salary >$< 'Rel8.desc')
-- @@@
over :: Window a b -> Partition a -> Window a b
over (Window (Opaleye.Windows (Opaleye.PackMap w))) (Partition p) =
  Window $ Opaleye.Windows $ Opaleye.PackMap $ \f ->
    w (\(o, p') -> f (o, p' <> p))
infixl 1 `over`


-- | Restricts a window function to operate only the group of rows that share
-- the same value(s) for the given expression(s).
partitionBy :: forall b a. EqTable b => (a -> b) -> Partition a
partitionBy f =
  Partition $ contramap (toColumns . f) $ getConst $
    htabulateA @(Columns b) $ \field ->
      Const $ Opaleye.partitionBy (toColumn . toPrimExpr . (`hfield` field))


-- | Controls the order in which rows are processed by window functions. This
-- does not need to match the ordering of the overall query.
orderPartitionBy :: Order a -> Partition a
orderPartitionBy (Order ordering) = Partition $ Opaleye.orderPartitionBy ordering
