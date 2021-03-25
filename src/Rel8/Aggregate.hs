{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Aggregate
  ( Aggregate(..)
  , Aggregator(..), unsafeMakeAggregate
  , Aggregates
  , Col( Aggregation )
  )
where

-- base
import Control.Applicative ( empty )
import Data.Foldable ( fold )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.Monoid ( First( First ), getFirst )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Opaque ( Opaque, Opaque1 )
import Rel8.Schema.Context ( Interpretation(..) )
import Rel8.Schema.Context.Label ( Labelable(..) )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, encodeTag, decodeTag, nullifier, unnullifier
  , runTag, unnull
  )
import Rel8.Schema.HTable ( hfield, htabulate, htabulateA, hspecs )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )

-- semigroupoids
import Data.Functor.Apply ( Apply, WrappedApplicative(..) )


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
  data Col Aggregate _spec where
    Aggregation :: ()
      => Aggregate (Expr a)
      -> Col Aggregate ('Spec labels necessity a)


instance Table Expr a => Table Aggregate (Aggregate a) where
  type Columns (Aggregate a) = Columns a
  type Context (Aggregate a) = Aggregate

  toColumns a = htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> Aggregation $ unDB . (`hfield` field) . toColumns <$> a
  fromColumns as = fmap fromColumns $ htabulateA $ \field ->
    case hfield as field of
      Aggregation a -> DB <$> a


instance Sql DBType a =>
  Recontextualize Aggregate Aggregate (Aggregate (Expr a)) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Aggregate Expr (Aggregate (Expr a)) (Expr a)


instance Sql DBType a =>
  Recontextualize Aggregate Identity (Aggregate (Expr a)) (Identity a)


instance Sql DBType a =>
  Recontextualize Aggregate Name (Aggregate (Expr a)) (Name a)


instance Sql DBType a =>
  Recontextualize Expr Aggregate (Expr a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Identity Aggregate (Identity a) (Aggregate (Expr a))


instance Sql DBType a =>
  Recontextualize Name Aggregate (Name a) (Aggregate (Expr a))


instance Labelable Aggregate where
  labeler (Aggregation aggregate) = Aggregation aggregate
  unlabeler (Aggregation aggregate) = Aggregation aggregate


instance Nullifiable Aggregate where
  encodeTag = Aggregation . groupByExpr
    where
      groupByExpr = unsafeMakeAggregate toPrimExpr fromPrimExpr Nothing
  decodeTag (Aggregation aggregate) = fold $ undoGroupBy aggregate
    where
      undoGroupBy = getFirst . foldInputs go
        where
          go Nothing = pure . fromPrimExpr
          go _ = const $ First empty

  nullifier tag SSpec {nullability} (Aggregation aggregate) = Aggregation $
    mapInputs (toPrimExpr . runTag nullability tag . fromPrimExpr) $
    runTag nullability tag <$> aggregate

  unnullifier _ SSpec {nullability} (Aggregation aggregate) =
    Aggregation $ unnull nullability <$> aggregate

  {-# INLINABLE encodeTag #-}
  {-# INLINABLE decodeTag #-}
  {-# INLINABLE nullifier #-}
  {-# INLINABLE unnullifier #-}


-- | @Aggregates a b@ means that the columns in @a@ are all 'Aggregate' 'Expr's
-- for the columns in @b@.
type Aggregates :: Type -> Type -> Constraint
class Recontextualize Aggregate Expr aggregates exprs => Aggregates aggregates exprs
instance Recontextualize Aggregate Expr aggregates exprs => Aggregates aggregates exprs
instance {-# OVERLAPPING #-} Aggregates (Opaque1 Aggregate Opaque) (Opaque1 Expr Opaque)


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
