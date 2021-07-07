{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
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
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Sql )
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
type Aggregate :: K.Context
newtype Aggregate a = Aggregate (Opaleye.Aggregator () (Expr a))


instance Sql DBType a => Table Aggregate (Aggregate a) where
  type Columns (Aggregate a) = HIdentity a
  type Context (Aggregate a) = Aggregate
  type FromExprs (Aggregate a) = a
  type Transpose to (Aggregate a) = to a

  toColumns = HIdentity
  fromColumns (HIdentity a) = a
  toResult = HIdentity . Identity
  fromResult (HIdentity (Identity a)) = a


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
