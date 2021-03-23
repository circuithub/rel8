{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Aggregate
  ( Aggregate(..), foldInputs, mapInputs
  , Aggregator(..), unsafeMakeAggregate
  , Col( Aggregation )
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- semigroupoids
import Data.Functor.Apply ( Apply, WrappedApplicative(..) )
import Rel8.Expr (Expr)
import Rel8.Schema.Spec


-- | An @Aggregate a@ describes how to aggregate @Table@s of type @a@. You can
-- unpack an @Aggregate@ back to @a@ by running it with 'aggregate'. As
-- @Aggregate@ is almost an 'Applicative' functor - but there is no 'pure'
-- operation. This means 'Aggregate' is an instance of 'Apply', and you can
-- combine @Aggregate@s using the @<.>@ combinator.
type Aggregate :: Type -> Type
newtype Aggregate a = Aggregate (Opaleye.Aggregator () a)
  deriving newtype Functor
  deriving Apply via (WrappedApplicative (Opaleye.Aggregator ()))


instance Interpretation Aggregate where
  data Col Aggregate :: Spec -> Type where
    Aggregation :: ()
      => Aggregate (Expr a)
      -> Col Aggregate ('Spec labels necessity dbType a)


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
