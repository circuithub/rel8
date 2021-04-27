{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Either
  ( EitherTable(..)
  , eitherTable, leftTable, rightTable
  , isLeftTable, isRightTable
  , aggregateEitherTable, nameEitherTable
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude hiding ( undefined )

-- rel8
import Rel8.Aggregate ( Aggregate, unsafeMakeAggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Context.Label
  ( Labelable
  , HLabelable, hlabeler, hunlabeler
  )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, ConstrainTag
  , HNullifiable, HConstrainTag
  , hencodeTag, hdecodeTag
  , hnullifier, hunnullifier
  )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import Rel8.Schema.Name ( Name )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Tag ( Tag(..), fromExpr, fromName )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ), isLeft, isRight )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>), liftF3 )
import Data.Functor.Bind ( Bind, (>>-) )


type EitherTable :: Type -> Type -> Type
data EitherTable a b = EitherTable
  { tag :: Tag "isRight" EitherTag
  , left :: a
  , right :: b
  }
  deriving stock Functor


instance Bifunctor EitherTable where
  bimap f g (EitherTable tag a b) = EitherTable tag (f a) (g b)


instance Table Expr a => Apply (EitherTable a) where
  EitherTable tag l1 f <.> EitherTable tag' l2 a =
    EitherTable (tag <> tag') (bool l1 l2 (isLeft (expr tag))) (f a)


instance Table Expr a => Applicative (EitherTable a) where
  pure = rightTable
  (<*>) = (<.>)


instance Table Expr a => Bind (EitherTable a) where
  EitherTable tag l1 a >>- f = case f a of
    EitherTable tag' l2 b ->
      EitherTable (tag <> tag') (bool l1 l2 (isRight (expr tag))) b


instance Table Expr a => Monad (EitherTable a) where
  (>>=) = (>>-)


instance (Table Expr a, Table Expr b) => Semigroup (EitherTable a b) where
  a <> b = bool a b (isRightTable a)


instance
  ( Table context a, Table context b
  , Labelable context, Nullifiable context, ConstrainTag context EitherTag
  ) =>
  Table context (EitherTable a b)
 where
  type Columns (EitherTable a b) = HEitherTable (Columns a) (Columns b)
  type Context (EitherTable a b) = Context a

  toColumns = toColumns2 toColumns toColumns
  fromColumns = fromColumns2 fromColumns fromColumns
  reify = liftA2 bimap reify reify
  unreify = liftA2 bimap unreify unreify


instance
  ( Nullifiable from, Labelable from, ConstrainTag from EitherTag
  , Nullifiable to, Labelable to, ConstrainTag to EitherTag
  , Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  ) =>
  Recontextualize from to (EitherTable a1 a2) (EitherTable b1 b2)


instance (EqTable a, EqTable b) => EqTable (EitherTable a b) where
  eqTable = toColumns2 id id (rightTableWith (eqTable @a) (eqTable @b))


instance (OrdTable a, OrdTable b) => OrdTable (EitherTable a b) where
  ordTable = toColumns2 id id (rightTableWith (ordTable @a) (ordTable @b))


isLeftTable :: EitherTable a b -> Expr Bool
isLeftTable = isLeft . expr . tag


isRightTable :: EitherTable a b -> Expr Bool
isRightTable = isRight . expr . tag


eitherTable :: Table Expr c
  => (a -> c) -> (b -> c) -> EitherTable a b -> c
eitherTable f g EitherTable {tag, left, right} =
  bool (f left) (g right) (isRight (expr tag))


leftTable :: Table Expr b => a -> EitherTable a b
leftTable a = EitherTable (fromExpr (litExpr IsLeft)) a undefined


rightTable :: Table Expr a => b -> EitherTable a b
rightTable = rightTableWith undefined


rightTableWith :: a -> b -> EitherTable a b
rightTableWith = EitherTable (fromExpr (litExpr IsRight))


aggregateEitherTable :: ()
  => (a -> Aggregate c)
  -> (b -> Aggregate d)
  -> EitherTable a b
  -> Aggregate (EitherTable c d)
aggregateEitherTable f g EitherTable {tag, left, right} =
  liftF3 EitherTable (tag <$ aggregate) (f left) (g right)
  where
    Tag {aggregator, expr} = tag
    aggregate = unsafeMakeAggregate toPrimExpr fromPrimExpr aggregator expr


nameEitherTable :: Name EitherTag -> a -> b -> EitherTable a b
nameEitherTable = EitherTable . fromName


toColumns2 ::
  ( HTable t
  , HTable u
  , HConstrainTag context EitherTag
  , HLabelable context
  , HNullifiable context
  )
  => (a -> t context)
  -> (b -> u context)
  -> EitherTable a b
  -> HEitherTable t u context
toColumns2 f g EitherTable {tag, left, right} = HEitherTable
  { htag
  , hleft = hlabel hlabeler $ hnullify (hnullifier tag isLeft) $ f left
  , hright = hlabel hlabeler $ hnullify (hnullifier tag isRight) $ g right
  }
  where
    htag = HIdentity (hencodeTag tag)


fromColumns2 ::
  ( HTable t
  , HTable u
  , HConstrainTag context EitherTag
  , HLabelable context
  , HNullifiable context
  )
  => (t context -> a)
  -> (u context -> b)
  -> HEitherTable t u context
  -> EitherTable a b
fromColumns2 f g HEitherTable {htag, hleft, hright} = EitherTable
  { tag
  , left = f $ runIdentity $
     hunnullify (\a -> pure . hunnullifier a) $
     hunlabel hunlabeler
     hleft
  , right = g $ runIdentity $
     hunnullify (\a -> pure . hunnullifier a) $
     hunlabel hunlabeler
     hright
  }
  where
    tag = hdecodeTag $ unHIdentity htag
