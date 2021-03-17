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

module Rel8.Table.Maybe
  ( MaybeTable(..)
  , maybeTable, nothingTable, justTable
  , isNothingTable, isJustTable
  , ($?)
  )
where

-- base
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude hiding ( null, repeat, undefined, zipWith )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( isNull, isNonNull, null, nullify )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Kind.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Context ( DB )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.Context.Nullify
  ( Nullifiable
  , encodeTag, decodeTag
  , nullifier, unnullifier
  )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Lifted
  ( Table1, Columns1, ConstrainContext1, fromColumns1, toColumns1
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type ( DBType )
import Rel8.Type.Tag ( MaybeTag( IsJust ) )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )


type MaybeTable :: Type -> Type
data MaybeTable a = MaybeTable
  { tag :: Expr 'Nullable MaybeTag
  , just :: a
  }
  deriving stock (Show, Functor)


instance Apply MaybeTable where
  MaybeTable tag f <.> MaybeTable tag' a = MaybeTable (tag <> tag') (f a)


instance Applicative MaybeTable where
  (<*>) = (<.>)
  pure = justTable


instance Bind MaybeTable where
  MaybeTable tag a >>- f = case f a of
    MaybeTable tag' b -> MaybeTable (tag <> tag') b


instance Monad MaybeTable where
  (>>=) = (>>-)


instance AltTable MaybeTable where
  ma@(MaybeTable tag a) <|>: MaybeTable tag' b = MaybeTable
    { tag = bool tag tag' condition
    , just = bool a b condition
    }
    where
      condition = isNothingTable ma


instance AlternativeTable MaybeTable where
  emptyTable = nothingTable


instance (Table a, Context a ~ DB, Semigroup a) => Semigroup (MaybeTable a) where
  ma <> mb = maybeTable mb (\a -> maybeTable ma (justTable . (a <>)) mb) ma


instance (Table a, Context a ~ DB, Semigroup a) => Monoid (MaybeTable a) where
  mempty = nothingTable


instance Table1 MaybeTable where
  type Columns1 MaybeTable = HMaybeTable
  type ConstrainContext1 MaybeTable = Nullifiable

  toColumns1 f MaybeTable {tag, just} = HMaybeTable
    { htag
    , hjust = hnullify (nullifier (isNonNull tag)) $ f just
    }
    where
      htag = HIdentity (encodeTag tag)

  fromColumns1 f HMaybeTable {htag = HIdentity htag, hjust} = MaybeTable
    { tag
    , just = f $ runIdentity $
        hunnullify (\a -> pure . unnullifier (isNonNull tag) a) hjust
    }
    where
      tag = decodeTag htag


instance
  ( Table a
  , Labelable (Context a), Nullifiable (Context a)
  ) => Table (MaybeTable a)
 where
  type Columns (MaybeTable a) = HMaybeTable (HLabel "Just" (Columns a))
  type Context (MaybeTable a) = Context a

  toColumns = toColumns1 (hlabel labeler . toColumns)
  fromColumns = fromColumns1 (fromColumns . hunlabel unlabeler)


instance
  ( Labelable from, Nullifiable from
  , Labelable to, Nullifiable to
  , Recontextualize from to a b
  ) => Recontextualize from to (MaybeTable a) (MaybeTable b)


isNothingTable :: MaybeTable a -> Expr 'NonNullable Bool
isNothingTable (MaybeTable tag _) = isNull tag


isJustTable :: MaybeTable a -> Expr 'NonNullable Bool
isJustTable (MaybeTable tag _) = isNonNull tag


maybeTable :: (Table b, Context b ~ DB) => b -> (a -> b) -> MaybeTable a -> b
maybeTable b f ma@(MaybeTable _ a) = bool b (f a) (isNothingTable ma)


nothingTable :: (Table a, Context a ~ DB) => MaybeTable a
nothingTable = MaybeTable null undefined


justTable :: a -> MaybeTable a
justTable = MaybeTable (nullify (litExpr IsJust))


($?) :: DBType b
  => (a -> Expr nullability b) -> MaybeTable a -> Expr 'Nullable b
f $? ma@(MaybeTable _ a) =
  boolExpr (nullify (f a)) null (isNothingTable ma)
infixl 4 $?
