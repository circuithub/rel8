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
  ( Aggregator(..)
  , Aggregate(..), unsafeMakeAggregate
  , Col( Aggregation )
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- profunctors
import Data.Profunctor ( Profunctor )

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Schema.Context ( Interpretation(..) )
import Rel8.Schema.Context.Label ( Labelable(..) )
import Rel8.Schema.HTable ( hfield, htabulate, htabulateA, hspecs )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec(..) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )

-- semigroupoids
import Data.Functor.Apply ( Apply, WrappedApplicative(..) )


-- | An @Aggregator i o@ describes how to aggregate @Table@s of type @i@ into
-- @Table@s of type @o@. You run @Aggregator@s with 'Rel8.aggregate'.
type Aggregator :: Type -> Type -> Type
newtype Aggregator a b = Aggregator { opaleyeAggregator :: Opaleye.Aggregator a b }
  deriving newtype (Functor, Applicative, Profunctor)
  deriving (Apply) via (WrappedApplicative (Opaleye.Aggregator a))


instance Interpretation (Aggregator i) where
  data Col (Aggregator i) _spec where
    Aggregation :: ()
      => Aggregator i (Expr a)
      -> Col (Aggregator i) ('Spec labels necessity a)


instance Table Expr o => Table (Aggregator i) (Aggregator i o) where
  type Columns (Aggregator i o) = Columns o
  type Context (Aggregator i o) = Aggregator i

  toColumns a = htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> Aggregation $ unDB . (`hfield` field) . toColumns <$> a
  fromColumns as = fmap fromColumns $ htabulateA $ \field ->
    case hfield as field of
      Aggregation a -> DB <$> a


instance Sql DBType a =>
  Recontextualize (Aggregator i) (Aggregator i) (Aggregator i (Expr a)) (Aggregator i (Expr a))


instance Sql DBType a =>
  Recontextualize (Aggregator i) Expr (Aggregator i (Expr a)) (Expr a)


instance Sql DBType a =>
  Recontextualize (Aggregator i) Identity (Aggregator i (Expr a)) (Identity a)


instance Sql DBType a =>
  Recontextualize (Aggregator i) Name (Aggregator i (Expr a)) (Name a)


instance Sql DBType a =>
  Recontextualize Expr (Aggregator i) (Expr a) (Aggregator i (Expr a))


instance Sql DBType a =>
  Recontextualize Identity (Aggregator i) (Identity a) (Aggregator i (Expr a))


instance Sql DBType a =>
  Recontextualize Name (Aggregator i) (Name a) (Aggregator i (Expr a))


instance Labelable (Aggregator i) where
  labeler (Aggregation aggregate) = Aggregation aggregate
  unlabeler (Aggregation aggregate) = Aggregation aggregate


type Aggregate :: Type
data Aggregate = Aggregate
  { operation :: Opaleye.AggrOp
  , ordering :: [Opaleye.OrderExpr]
  , distinction :: Opaleye.AggrDistinct
  }


unsafeMakeAggregate :: ()
  => (input -> Opaleye.PrimExpr)
  -> (Opaleye.PrimExpr -> output)
  -> Maybe Aggregate
  -> (a -> input)
  -> Aggregator a output
unsafeMakeAggregate input output aggregator toExpr =
  Aggregator $ Opaleye.Aggregator $ Opaleye.PackMap $ \f a ->
    output <$> f (tuplize <$> aggregator, input (toExpr a))
  where
    tuplize Aggregate {operation, ordering, distinction} =
      (operation, ordering, distinction)
