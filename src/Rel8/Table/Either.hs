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
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude hiding ( undefined )

-- rel8
import Rel8.Aggregate ( Aggregate, unsafeMakeAggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler, hlabeler )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, ConstrainTag
  , hencodeTag, hdecodeTag
  , hnullifier, hunnullifier
  )
import Rel8.Schema.HTable.Either ( HEitherTable(..), HEitherNullifiable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import Rel8.Schema.Name ( Name )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Lifted
  ( Table2, Columns2, ConstrainHContext2, fromColumns2, toColumns2
  )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Tag ( Tag(..), fromIdentity, fromName )
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


instance Table2 EitherTable where
  type Columns2 EitherTable = HEitherTable
  type ConstrainHContext2 EitherTable = HEitherNullifiable

  toColumns2 f g EitherTable {tag, left, right} = HEitherTable
    { htag
    , hleft = hnullify (hnullifier tag (== IsLeft) isLeft) $ f left
    , hright = hnullify (hnullifier tag (== IsRight) isRight) $ g right
    }
    where
      htag = HIdentity (hencodeTag tag)

  fromColumns2 f g HEitherTable {htag = htag, hleft, hright} =
    EitherTable
      { tag
      , left = f $ runIdentity $
          hunnullify (\a -> pure . hunnullifier a) hleft
      , right = g $ runIdentity $
          hunnullify (\a -> pure . hunnullifier a) hright
      }
    where
      tag = hdecodeTag $ unHIdentity htag

  {-# INLINABLE fromColumns2 #-}
  {-# INLINABLE toColumns2 #-}


instance
  ( Table context a, Table context b
  , Labelable context, Nullifiable context, ConstrainTag context EitherTag
  ) =>
  Table context (EitherTable a b)
 where
  type Columns (EitherTable a b) =
    HEitherTable (HLabel "Left" (Columns a)) (HLabel "Right" (Columns b))
  type Context (EitherTable a b) = Context a

  toColumns =
    toColumns2
      (hlabel labeler . toColumns)
      (hlabel labeler . toColumns)
  fromColumns =
    fromColumns2
      (fromColumns . hunlabel unlabeler)
      (fromColumns . hunlabel unlabeler)


instance
  ( Nullifiable from, Labelable from, ConstrainTag from EitherTag
  , Nullifiable to, Labelable to, ConstrainTag to EitherTag
  , Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  ) =>
  Recontextualize from to (EitherTable a1 a2) (EitherTable b1 b2)


instance (EqTable a, EqTable b) => EqTable (EitherTable a b) where
  eqTable =
    toColumns2 (hlabel hlabeler) (hlabel hlabeler)
      (rightTableWith (eqTable @a) (eqTable @b))


instance (OrdTable a, OrdTable b) => OrdTable (EitherTable a b) where
  ordTable =
    toColumns2 (hlabel hlabeler) (hlabel hlabeler)
      (rightTableWith (ordTable @a) (ordTable @b))


isLeftTable :: EitherTable a b -> Expr Bool
isLeftTable = isLeft . expr . tag


isRightTable :: EitherTable a b -> Expr Bool
isRightTable = isRight . expr . tag


eitherTable :: Table Expr c
  => (a -> c) -> (b -> c) -> EitherTable a b -> c
eitherTable f g EitherTable {tag, left, right} =
  bool (f left) (g right) (isRight (expr tag))


leftTable :: Table Expr b => a -> EitherTable a b
leftTable a = EitherTable (fromIdentity IsLeft) a undefined


rightTable :: Table Expr a => b -> EitherTable a b
rightTable = rightTableWith undefined


rightTableWith :: a -> b -> EitherTable a b
rightTableWith = EitherTable (fromIdentity IsRight)


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
