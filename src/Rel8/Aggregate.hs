{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Aggregate
  ( Aggregate(..), foldInputs, mapInputs
  , Aggregator(..), unsafeMakeAggregate
  , Aggregates
  , Col( A, unA )
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- rel8
import Rel8.Expr ( Expr, Col( E, unE ) )
import Rel8.Schema.Context ( Interpretation(..) )
import Rel8.Schema.Context.Label ( Labelable(..) )
import Rel8.Schema.HTable ( hfield, htabulate, htabulateA, hspecs )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )


-- | An @Aggregate a@ describes how to aggregate @Table@s of type @a@. You can
-- unpack an @Aggregate@ back to @a@ by running it with 'Rel8.aggregate'. As
-- @Aggregate@ is almost an 'Applicative' functor - but there is no 'pure'
-- operation. This means 'Aggregate' is an instance of 'Apply', and you can
-- combine @Aggregate@s using the @<.>@ combinator.
type Aggregate :: k -> Type
data Aggregate a where
  Aggregate :: !(Opaleye.Aggregator () a) -> Aggregate a


instance Functor Aggregate where
  fmap f (Aggregate a) = Aggregate (fmap f a)


instance Apply Aggregate where
  Aggregate f <.> Aggregate a = Aggregate (f <*> a)


instance Interpretation Aggregate where
  data Col Aggregate _spec where
    A :: ()
      => { unA :: !(Aggregate (Expr a)) }
      -> Col Aggregate ('Spec labels necessity a)


instance Table Expr a => Table Aggregate (Aggregate a) where
  type Columns (Aggregate a) = Columns a
  type Context (Aggregate a) = Aggregate

  toColumns a = htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> A $ unE . (`hfield` field) . toColumns <$> a
  fromColumns as = fmap fromColumns $ htabulateA $ \field ->
    case hfield as field of
      A a -> E <$> a

  reify = notReify
  unreify = notReify


instance Sql DBType a =>
  Recontextualize Aggregate Aggregate (Aggregate (Expr a)) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Aggregate Expr (Aggregate (Expr a)) (Expr a)


instance Sql DBType a =>
  Recontextualize Aggregate Result (Aggregate (Expr a)) (Identity a)


instance Sql DBType a =>
  Recontextualize Aggregate Name (Aggregate (Expr a)) (Name a)


instance Sql DBType a =>
  Recontextualize Expr Aggregate (Expr a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Result Aggregate (Identity a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Name Aggregate (Name a) (Aggregate (Expr a))


instance Labelable Aggregate where
  labeler (A aggregate) = A aggregate
  unlabeler (A aggregate) = A aggregate


-- | @Aggregates a b@ means that the columns in @a@ are all 'Aggregate' 'Expr's
-- for the columns in @b@.
type Aggregates :: Type -> Type -> Constraint
class Recontextualize Aggregate Expr aggregates exprs => Aggregates aggregates exprs
instance Recontextualize Aggregate Expr aggregates exprs => Aggregates aggregates exprs


foldInputs :: Monoid b
  => (Maybe Aggregator -> Opaleye.PrimExpr -> b) -> Aggregate a -> b
foldInputs f (Aggregate (Opaleye.Aggregator (Opaleye.PackMap agg))) =
  getConst $ flip agg () $ \(aggregator, a) ->
    Const $ f (detuplize <$> aggregator) a
  where
    detuplize (operation, ordering, distinction) =
      Aggregator {operation, ordering, distinction}


mapInputs :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr) -> Aggregate a -> Aggregate a
mapInputs transform (Aggregate (Opaleye.Aggregator (Opaleye.PackMap agg))) =
  Aggregate $ Opaleye.Aggregator $ Opaleye.PackMap $ agg . \f input ->
    f (fmap transform input)


type Aggregator :: Type
data Aggregator = Aggregator
  { operation :: Opaleye.AggrOp
  , ordering :: [Opaleye.OrderExpr]
  , distinction :: Opaleye.AggrDistinct
  }


unsafeMakeAggregate :: ()
  => (input -> Opaleye.PrimExpr)
  -> (Opaleye.PrimExpr -> output)
  -> Maybe Aggregator
  -> input
  -> Aggregate output
unsafeMakeAggregate input output aggregator expr =
  Aggregate $ Opaleye.Aggregator $ Opaleye.PackMap $ \f _ ->
    output <$> f (tuplize <$> aggregator, input expr)
  where
    tuplize Aggregator {operation, ordering, distinction} =
      (operation, ordering, distinction)
