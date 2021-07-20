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
  ( Aggregate(..), zipOutputs
  , Aggregator(..), unsafeMakeAggregate
  , Aggregates
  )
where

-- base
import Control.Applicative ( liftA2 )
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


-- | 'Aggregate' is a special context used by 'Rel8.aggregate'.
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


-- | @Aggregates a b@ means that the columns in @a@ are all 'Aggregate's
-- for the 'Expr' columns in @b@.
type Aggregates :: Type -> Type -> Constraint
class Transposes Aggregate Expr aggregates exprs => Aggregates aggregates exprs
instance Transposes Aggregate Expr aggregates exprs => Aggregates aggregates exprs


zipOutputs :: ()
  => (Expr a -> Expr b -> Expr c) -> Aggregate a -> Aggregate b -> Aggregate c
zipOutputs f (Aggregate a) (Aggregate b) = Aggregate (liftA2 f a b)


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
