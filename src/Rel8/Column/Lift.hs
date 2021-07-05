{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Column.Lift
  ( Lift, ALift(..)
  )
where

-- base
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Generic.Rel8able ( GColumns )
import Rel8.Kind.Context ( Reifiable(..), SContext(..) )
import Rel8.Schema.Context ( Col )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Reify ( Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result, absurd )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Unreify, reify, unreify, coherence, congruence
  )
import Rel8.Table.Rel8able ()
import Rel8.Table.HKD ( HKD( HKD ), HKDable, fromHKD, toHKD )
import Rel8.Table.Recontextualize ( Recontextualize )


type Lift :: K.Context -> Type -> Type
type family Lift context a where
  Lift (Reify context) a = ALift context a
  Lift Result a = a
  Lift context a = HKD a context


type ALift :: K.Context -> Type -> Type
newtype ALift context a = ALift
  { unALift :: Lift context a
  }


instance (Reifiable context, HKDable a) =>
  Table (Reify context) (ALift context a)
 where
  type Context (ALift context a) = Reify context
  type Columns (ALift context a) = GColumns (HKD a)
  type Unreify (ALift context a) = Lift context a

  fromColumns = sfromColumnsLift contextSing
  toColumns = stoColumnsLift contextSing
  reify _ = ALift
  unreify _ (ALift a) = a

  coherence = case contextSing @context of
    SAggregate -> \Refl _ -> Refl
    SExpr -> \Refl _ -> Refl
    SName -> \Refl _ -> Refl
    SResult -> \Refl -> absurd
    SReify _ -> \Refl _ -> Refl

  congruence = case contextSing @context of
    SAggregate -> \_ _ -> Refl
    SExpr -> \_ _ -> Refl
    SName -> \_ _ -> Refl
    SResult -> \Refl -> absurd
    SReify _ -> \_ _ -> Refl


instance (Reifiable context, Reifiable context', HKDable a) =>
  Recontextualize
    (Reify context)
    (Reify context')
    (ALift context a)
    (ALift context' a)


sfromColumnsLift :: HKDable a
  => SContext context
  -> GColumns (HKD a) (Col (Reify context))
  -> ALift context a
sfromColumnsLift = \case
  SAggregate -> ALift . fromColumns . hunreify
  SExpr -> ALift . fromColumns . hunreify
  SName -> ALift . fromColumns . hunreify
  SResult -> ALift . fromHKD . HKD . hunreify
  SReify context -> ALift . sfromColumnsLift context . hunreify


stoColumnsLift :: HKDable a
  => SContext context
  -> ALift context a
  -> GColumns (HKD a) (Col (Reify context))
stoColumnsLift = \case
  SAggregate -> hreify . toColumns . unALift
  SExpr -> hreify . toColumns . unALift
  SName -> hreify . toColumns . unALift
  SResult -> hreify . (\(HKD a) -> a) . toHKD . unALift
  SReify context -> hreify . stoColumnsLift context . unALift
