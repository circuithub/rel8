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
import Rel8.Expr ( Expr )
import Rel8.Schema.Context ( Interpretation(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..), HType )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify, coherence, congruence
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


-- | An @Aggregate a@ describes how to aggregate @Table@s of type @a@. You can
-- unpack an @Aggregate@ back to @a@ by running it with 'Rel8.aggregate'. As
-- @Aggregate@ is almost an 'Applicative' functor - but there is no 'pure'
-- operation. This means 'Aggregate' is an instance of 'Apply', and you can
-- combine @Aggregate@s using the @<.>@ combinator.
type Aggregate :: k -> Type
data Aggregate a where
  Aggregate :: !(Opaleye.Aggregator () (Expr a)) -> Aggregate a


instance Interpretation Aggregate where
  data Col Aggregate _spec where
    A :: ()
      => { unA :: !(Aggregate a) }
      -> Col Aggregate ('Spec a)


instance Sql DBType a => Table Aggregate (Aggregate a) where
  type Columns (Aggregate a) = HType a
  type Context (Aggregate a) = Aggregate

  toColumns = HIdentity . A
  fromColumns (HIdentity (A a)) = a

  reify = notReify
  unreify = notReify
  coherence = notReify
  congruence = notReify


instance Sql DBType a =>
  Recontextualize Aggregate Aggregate (Aggregate a) (Aggregate a)


instance Sql DBType a =>
  Recontextualize Aggregate Expr (Aggregate a) (Expr a)


instance Sql DBType a =>
  Recontextualize Aggregate Result (Aggregate a) (Identity a)


instance Sql DBType a =>
  Recontextualize Aggregate Name (Aggregate a) (Name a)


instance Sql DBType a =>
  Recontextualize Expr Aggregate (Expr a) (Aggregate a)


instance Sql DBType a =>
  Recontextualize Result Aggregate (Identity a) (Aggregate a)


instance Sql DBType a =>
  Recontextualize Name Aggregate (Name a) (Aggregate a)


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
  => (Expr input -> Opaleye.PrimExpr)
  -> (Opaleye.PrimExpr -> Expr output)
  -> Maybe Aggregator
  -> Expr input
  -> Aggregate output
unsafeMakeAggregate input output aggregator expr =
  Aggregate $ Opaleye.Aggregator $ Opaleye.PackMap $ \f _ ->
    output <$> f (tuplize <$> aggregator, input expr)
  where
    tuplize Aggregator {operation, ordering, distinction} =
      (operation, ordering, distinction)
