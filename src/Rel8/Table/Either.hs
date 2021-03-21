{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Either
  ( EitherTable(..)
  , eitherTable, leftTable, rightTable
  , isLeftTable, isRightTable
  )
where

-- base
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude hiding ( undefined )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( litPrimExpr )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Schema.Context ( DB )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, NullifiableEq
  , encodeTag, decodeTag
  , nullifier, unnullifier
  )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Lifted
  ( Table1, Columns1, ConstrainContext1, fromColumns1, toColumns1
  , Table2, Columns2, ConstrainContext2, fromColumns2, toColumns2
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ), isLeft, isRight )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )


type EitherTable :: Type -> Type -> Type
data EitherTable a b = EitherTable
  { tag :: Expr 'NonNullable EitherTag
  , left :: a
  , right :: b
  }
  deriving stock (Show, Functor)


instance Bifunctor EitherTable where
  bimap f g (EitherTable tag a b) = EitherTable tag (f a) (g b)


instance Table DB a => Apply (EitherTable a) where
  EitherTable tag l1 f <.> EitherTable tag' l2 a =
    EitherTable (tag <> tag') (bool l1 l2 (isLeft tag)) (f a)


instance Table DB a => Applicative (EitherTable a) where
  pure = rightTable
  (<*>) = (<.>)


instance Table DB a => Bind (EitherTable a) where
  EitherTable tag l1 a >>- f = case f a of
    EitherTable tag' l2 b ->
      EitherTable (tag <> tag') (bool l1 l2 (isRight tag)) b


instance Table DB a => Monad (EitherTable a) where
  (>>=) = (>>-)


instance (Table DB a, Table DB b) => Semigroup (EitherTable a b) where
  a <> b = bool a b (isRightTable a)


instance Table2 EitherTable where
  type Columns2 EitherTable = HEitherTable
  type ConstrainContext2 EitherTable = Nullifiable

  toColumns2 f g EitherTable {tag, left, right} = HEitherTable
    { htag
    , hleft = hnullify (nullifier (isLeft tag)) $ f left
    , hright = hnullify (nullifier (isRight tag)) $ g right
    }
    where
      htag = HIdentity (encodeTag tag)

  fromColumns2 f g HEitherTable {htag = htag, hleft, hright} =
    EitherTable
      { tag
      , left = f $ runIdentity $
          hunnullify (\a -> pure . unnullifier (isLeft tag) a) hleft
      , right = g $ runIdentity $
          hunnullify (\a -> pure . unnullifier (isRight tag) a) hright
      }
    where
      tag = decodeTag $ unHIdentity htag

  {-# INLINABLE fromColumns2 #-}
  {-# INLINABLE toColumns2 #-}


instance Table context a => Table1 (EitherTable a) where
  type Columns1 (EitherTable a) = HEitherTable (Columns a)
  type ConstrainContext1 (EitherTable a) = NullifiableEq (Context a)

  toColumns1 = toColumns2 toColumns
  fromColumns1 = fromColumns2 fromColumns


instance
  ( Table context a, Table context b
  , Labelable context, Nullifiable context
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
  ( Nullifiable from, Labelable from
  , Nullifiable to, Labelable to
  , Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  ) =>
  Recontextualize from to (EitherTable a1 a2) (EitherTable b1 b2)


isLeftTable :: EitherTable a b -> Expr 'NonNullable Bool
isLeftTable = isLeft . tag


isRightTable :: EitherTable a b -> Expr 'NonNullable Bool
isRightTable = isRight . tag


eitherTable :: Table DB c
  => (a -> c) -> (b -> c) -> EitherTable a b -> c
eitherTable f g EitherTable {tag, left, right} =
  bool (f left) (g right) (isRight tag)


leftTable :: Table DB b => a -> EitherTable a b
leftTable a = EitherTable (litPrimExpr IsLeft) a undefined


rightTable :: Table DB a => b -> EitherTable a b
rightTable = EitherTable (litPrimExpr IsRight) undefined
