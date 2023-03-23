{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Nullify
  ( Nullify
  , aggregateNullify
  , guard
  , isNull
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude

-- comonad
import Control.Comonad ( Comonad, duplicate, extract, ComonadApply, (<@>) )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (||.), false )
import qualified Rel8.Expr.Null as Expr
import Rel8.Kind.Context ( Reifiable, contextSing )
import Rel8.Schema.Context.Nullify
  ( Nullifiability( NAggregate, NExpr )
  , NonNullifiability
  , Nullifiable, nullifiability
  , nullifiableOrNot, absurd
  , guarder
  , nullifier
  , unnullifier
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Nullify
  ( HNullify, hnulls, hnullify, hunnullify
  , hguard
  , hproject
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Nullity( NotNull, Null ) )
import qualified Rel8.Schema.Result as R
import Rel8.Table
  ( Table, Columns, Context, toColumns, fromColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Schema.Spec ( Spec( Spec, nullity ) )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Projection ( Projectable, apply, project )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>), liftF2 )
import Data.Functor.Bind ( Bind, (>>-) )
import Data.Functor.Extend ( Extend, duplicated )


type Nullify :: K.Context -> Type -> Type
data Nullify context a
  = Table (Nullifiability context) a
  | Fields (NonNullifiability context) (HNullify (Columns a) (Context a))


instance Projectable (Nullify context) where
  project f = \case
    Table nullifiable a -> Table nullifiable (fromColumns (apply f (toColumns a)))
    Fields nonNullifiable a -> Fields nonNullifiable (hproject (apply f) a)


instance Nullifiable context => Functor (Nullify context) where
  fmap f = \case
    Table nullifiable a -> Table nullifiable (f a)
    Fields notNullifiable _ -> absurd nullifiability notNullifiable


instance Nullifiable context => Foldable (Nullify context) where
  foldMap f = \case
    Table _ a -> f a
    Fields notNullifiable _ -> absurd nullifiability notNullifiable


instance Nullifiable context => Traversable (Nullify context) where
  traverse f = \case
    Table nullifiable a -> Table nullifiable <$> f a
    Fields notNullifiable _ -> absurd nullifiability notNullifiable


instance Nullifiable context => Apply (Nullify context) where
  liftF2 f = \case
    Table nullifiable a -> \case
      Table _ b -> Table nullifiable (f a b)
      Fields notNullifiable _ -> absurd nullifiable notNullifiable
    Fields notNullifiable _ -> absurd nullifiability notNullifiable


instance Nullifiable context => Applicative (Nullify context) where
  pure = Table nullifiability
  liftA2 = liftF2


instance Nullifiable context => Bind (Nullify context) where
  Table _ a >>- f = f a
  Fields notNullifiable _ >>- _ = absurd nullifiability notNullifiable


instance Nullifiable context => Monad (Nullify context) where
  (>>=) = (>>-)


instance Nullifiable context => Extend (Nullify context) where
  duplicated = \case
    Table nullifiable a -> Table nullifiable (Table nullifiable a)
    Fields notNullifiable _ -> absurd nullifiability notNullifiable


instance Nullifiable context => Comonad (Nullify context) where
  extract = \case
    Table _ a -> a
    Fields notNullifiable _ -> absurd nullifiability notNullifiable
  duplicate = duplicated


instance Nullifiable context => ComonadApply (Nullify context) where
  (<@>) = (<.>)


instance (Table context a, Reifiable context, context ~ context') =>
  Table context' (Nullify context a)
 where
  type Columns (Nullify context a) = HNullify (Columns a)
  type Context (Nullify context a) = Context a
  type FromExprs (Nullify context a) = Maybe (FromExprs a)
  type Transpose to (Nullify context a) = Nullify to (Transpose to a)

  fromColumns = case nullifiableOrNot contextSing of
    Left notNullifiable -> Fields notNullifiable
    Right nullifiable ->
      Table nullifiable .
      fromColumns .
      runIdentity .
      hunnullify (\spec -> pure . unnullifier nullifiable spec)

  toColumns = \case
    Table nullifiable a -> hnullify (nullifier nullifiable) (toColumns a)
    Fields _ a -> a

  fromResult = fmap (fromResult @_ @a) . hunnullify R.unnullifier

  toResult =
    maybe (hnulls (const R.null)) (hnullify R.nullifier) .
    fmap (toResult @_ @a)


instance (EqTable a, context ~ Expr) => EqTable (Nullify context a) where
  eqTable = hnullify (\_ Dict -> Dict) (eqTable @a)


instance (OrdTable a, context ~ Expr) => OrdTable (Nullify context a) where
  ordTable = hnullify (\_ Dict -> Dict) (ordTable @a)


aggregateNullify :: ()
  => (exprs -> aggregates)
  -> Nullify Expr exprs
  -> Nullify Aggregate aggregates
aggregateNullify f = \case
  Table _ a -> Table NAggregate (f a)
  Fields notNullifiable _ -> absurd NExpr notNullifiable


guard :: (Reifiable context, HTable t)
  => context tag
  -> (tag -> Bool)
  -> (Expr tag -> Expr Bool)
  -> HNullify t context
  -> HNullify t context
guard tag isNonNull isNonNullExpr =
  hguard (guarder contextSing tag isNonNull isNonNullExpr)


isNull :: forall a. Table Expr a => Nullify Expr a -> Expr Bool
isNull =
  maybe false getAny .
  getConst .
  hunnullify (\Spec {nullity} a -> Const $ case nullity of
    NotNull -> Just $ Any $ Expr.isNull a
    Null -> Nothing) .
  toColumns


newtype Any = Any
  { getAny :: Expr Bool
  }


instance Semigroup Any where
  Any a <> Any b = Any (a ||. b)
