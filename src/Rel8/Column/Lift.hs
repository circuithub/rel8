{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column.Lift
  ( Lift, ALift(..)
  )
where

-- base
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..), runIdentity )
import Data.Kind ( Type )
import GHC.Generics ( Rep )
import Prelude

-- higgledy
import Data.Generic.HKD ( Construct, HKD(..), construct, deconstruct )

-- rel8
import Rel8.Aggregate ( Col(..), Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Generic.HKD ( GTable, GColumns, gfromColumns, gtoColumns )
import Rel8.Kind.Context ( Reifiable(..), SContext(..) )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import Rel8.Schema.Insert ( Insert, Col(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..) )
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )


type Lift :: K.Context -> Type -> Type
type family Lift context a where
  Lift (Reify context) a = ALift context a
  Lift Aggregate a = HKD a (Compose Aggregate Expr)
  Lift Expr a = HKD a Expr
  Lift Insert a = HKD a Expr
  Lift Name a = HKD a Name
  Lift Result a = a


type ALift :: K.Context -> Type -> Type
newtype ALift context a = ALift
  { unALift :: Lift context a
  }


instance
  ( Reifiable context
  , GTable (Rep a)
  , Construct Identity a
  )
  => Table (Reify context) (ALift context a)
 where
  type Context (ALift context a) = Reify context
  type Columns (ALift context a) = GColumns (Rep a)
  type Unreify (ALift context a) = Lift context a

  fromColumns = sfromColumnsLift contextSing
  toColumns = stoColumnsLift contextSing
  reify _ = ALift
  unreify _ (ALift a) = a


instance
  ( Reifiable context
  , Reifiable context'
  , GTable (Rep a)
  , Construct Identity a
  )
  => Recontextualize
    (Reify context)
    (Reify context')
    (ALift context a)
    (ALift context' a)


sfromColumnsLift :: forall a context. (GTable (Rep a), Construct Identity a)
  => SContext context
  -> GColumns (Rep a) (Col (Reify context))
  -> ALift context a
sfromColumnsLift = \case
  SAggregate ->
    ALift .
    HKD .
    gfromColumns (\(HType (Aggregation a)) -> Compose a) .
    hunreify
  SExpr -> ALift . fromColumns . hunreify
  SInsert ->
    ALift .
    HKD .
    gfromColumns (\(HType (RequiredInsert a)) -> a) .
    hunreify
  SName -> ALift . fromColumns . hunreify
  SResult -> ALift . runIdentity . construct . fromColumns . hunreify
  SReify context -> ALift . sfromColumnsLift context . hunreify


stoColumnsLift :: forall a context. (GTable (Rep a), Construct Identity a)
  => SContext context
  -> ALift context a
  -> GColumns (Rep a) (Col (Reify context))
stoColumnsLift = \case
  SAggregate ->
    hreify .
    gtoColumns (\(Compose a) -> HType (Aggregation a)) .
    runHKD .
    unALift
  SExpr -> hreify . toColumns . unALift
  SInsert ->
    hreify .
    gtoColumns (HType . RequiredInsert) .
    runHKD .
    unALift
  SName -> hreify . toColumns . unALift
  SResult -> hreify . toColumns . deconstruct @Identity . unALift
  SReify context -> hreify . stoColumnsLift context . unALift
