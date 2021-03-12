{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
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
import Rel8.Expr ( Expr, litExpr )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Kind.Nullability ( Nullability( NonNullable ), nullabilitySing )
import Rel8.Schema.Context ( DB )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, NullifiableEq
  , encodeTag, decodeTag
  , nullifier, unnullifier
  )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , Compatible
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Lifted
  ( Table1, Columns1, ConstrainContext1, fromColumns1, toColumns1
  , Table2, Columns2, ConstrainContext2, fromColumns2, toColumns2
  )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type ( typeInformation )
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
  deriving stock Functor


instance Bifunctor EitherTable where
  bimap f g (EitherTable tag a b) = EitherTable tag (f a) (g b)


instance (Table a, Context a ~ DB) => Apply (EitherTable a) where
  EitherTable tag l1 f <.> EitherTable tag' l2 a =
    EitherTable (tag <> tag') (bool l1 l2 (tag ==. litExpr IsLeft)) (f a)


instance (Table a, Context a ~ DB) => Applicative (EitherTable a) where
  pure = rightTable
  (<*>) = (<.>)


instance (Table a, Context a ~ DB) => Bind (EitherTable a) where
  EitherTable tag l1 a >>- f = case f a of
    EitherTable tag' l2 b ->
      EitherTable (tag <> tag') (bool l1 l2 (tag ==. litExpr IsLeft)) b


instance (Table a, Context a ~ DB) => Monad (EitherTable a) where
  (>>=) = (>>-)


instance (Table a, Context a ~ DB, Table b, Context b ~ DB) =>
  Semigroup (EitherTable a b)
 where
  a <> b = bool a b (isRightTable a)


instance Table2 EitherTable where
  type Columns2 EitherTable = HEitherTable
  type ConstrainContext2 EitherTable = Nullifiable

  toColumns2 f g EitherTable {tag, left, right} = HEitherTable
    { htag
    , hleft = hnullify (nullifier "Left" (isLeft tag)) (f left)
    , hright = hnullify (nullifier "Right" (isRight tag)) (g right)
    }
    where
      htag =
        HIdentity (encodeTag "isRight" nullabilitySing typeInformation tag)

  fromColumns2 f g HEitherTable {htag = HIdentity htag, hleft, hright} =
    EitherTable
      { tag
      , left = f $ runIdentity $
          hunnullify (\a -> pure . unnullifier "Left" (isLeft tag) a) hleft
      , right = g $ runIdentity $
          hunnullify (\a -> pure . unnullifier "Right" (isRight tag) a) hright
      }
    where
      tag = decodeTag "isRight" nullabilitySing typeInformation htag


instance Table a => Table1 (EitherTable a) where
  type Columns1 (EitherTable a) = HEitherTable (Columns a)
  type ConstrainContext1 (EitherTable a) = NullifiableEq (Context a)

  toColumns1 = toColumns2 toColumns
  fromColumns1 = fromColumns2 fromColumns


instance (Table a, Table b, Compatible a b, Nullifiable (Context a)) =>
  Table (EitherTable a b)
 where
  type Columns (EitherTable a b) = HEitherTable (Columns a) (Columns b)
  type Context (EitherTable a b) = Context a

  toColumns = toColumns2 toColumns toColumns
  fromColumns = fromColumns2 fromColumns fromColumns


isLeftTable :: EitherTable a b -> Expr 'NonNullable Bool
isLeftTable = isLeft . tag


isRightTable :: EitherTable a b -> Expr 'NonNullable Bool
isRightTable = isRight . tag


eitherTable :: (Table c, Context c ~ DB)
  => (a -> c) -> (b -> c) -> EitherTable a b -> c
eitherTable f g EitherTable {tag, left, right} =
  bool (f left) (g right) (isRight tag)


leftTable :: (Table b, Context b ~ DB) => a -> EitherTable a b
leftTable a = EitherTable (litExpr IsLeft) a undefined


rightTable :: (Table a, Context a ~ DB) => b -> EitherTable a b
rightTable = EitherTable (litExpr IsRight) undefined
