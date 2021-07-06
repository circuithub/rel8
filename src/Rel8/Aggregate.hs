{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Aggregate
  ( Aggregate(..), foldInputs, mapInputs
  , Aggregator(..), unsafeMakeAggregate
  , Aggregates
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Schema.Context.Lower ( Lower )
import Rel8.Schema.HTable.Identity ( HIdentity(..), HType )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Result ( Result( R ) )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.Transpose ( Transposes )
import Rel8.Type ( DBType )


-- | An @Aggregate a@ describes how to aggregate @Table@s of type @a@. You can
-- unpack an @Aggregate@ back to @a@ by running it with 'Rel8.aggregate'. As
-- @Aggregate@ is almost an 'Applicative' functor - but there is no 'pure'
-- operation. This means 'Aggregate' is an instance of 'Apply', and you can
-- combine @Aggregate@s using the @<.>@ combinator.
type Aggregate :: k -> Type
data Aggregate a where
  Aggregate :: forall (a :: Type). !(Opaleye.Aggregator () (Expr a)) -> Aggregate a
  A :: { unA :: !(Aggregate a) } -> Aggregate ('Spec a)


type instance Lower Aggregate = Aggregate


instance Sql DBType a => Table Aggregate (Aggregate a) where
  type Columns (Aggregate a) = HType a
  type Context (Aggregate a) = Aggregate
  type FromExprs (Aggregate a) = a
  type Transpose to (Aggregate a) = Lower to a

  toColumns = HIdentity . A
  fromColumns (HIdentity (A a)) = a
  toResult = HIdentity . R
  fromResult (HIdentity (R a)) = a


-- | @Aggregates a b@ means that the columns in @a@ are all 'Aggregate' 'Expr's
-- for the columns in @b@.
type Aggregates :: Type -> Type -> Constraint
class Transposes Aggregate Expr aggregates exprs => Aggregates aggregates exprs
instance Transposes Aggregate Expr aggregates exprs => Aggregates aggregates exprs


foldInputs :: forall (a :: Type) (b :: Type). Monoid b
  => (Maybe Aggregator -> Opaleye.PrimExpr -> b) -> Aggregate a -> b
foldInputs f (Aggregate (Opaleye.Aggregator (Opaleye.PackMap agg))) =
  getConst $ flip agg () $ \(aggregator, a) ->
    Const $ f (detuplize <$> aggregator) a
  where
    detuplize (operation, ordering, distinction) =
      Aggregator {operation, ordering, distinction}


mapInputs :: forall (a :: Type). ()
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


unsafeMakeAggregate :: forall (input :: Type) (output :: Type). ()
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
