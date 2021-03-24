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
import Rel8.Schema.Nullability
  ( Nullify
  , Nullability( Nullable, NonNullable )
  , Sql, nullabilization
  )
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


-- | @MaybeTable t@ is the table @t@, but as the result of an outer join. If
-- the outer join fails to match any rows, this is essentialy @Nothing@, and if
-- the outer join does match rows, this is like @Just@. Unfortunately, SQL
-- makes it impossible to distinguish whether or not an outer join matched any
-- rows based generally on the row contents - if you were to join a row
-- entirely of nulls, you can't distinguish if you matched an all null row, or
-- if the match failed.  For this reason @MaybeTable@ contains an extra field -
-- a "nullTag" - to track whether or not the outer join produced any rows.
type MaybeTable :: Type -> Type
data MaybeTable a = MaybeTable
  { tag :: Expr (Maybe MaybeTag)
  , just :: a
  }
  deriving stock (Show, Functor)


instance Apply MaybeTable where
  MaybeTable tag f <.> MaybeTable tag' a = MaybeTable (tag <> tag') (f a)


-- | Has the same behavior as the @Applicative@ instance for @Maybe@. See also:
-- 'traverseMaybeTable'.
instance Applicative MaybeTable where
  (<*>) = (<.>)
  pure = justTable


instance Bind MaybeTable where
  MaybeTable tag a >>- f = case f a of
    MaybeTable tag' b -> MaybeTable (tag <> tag') b


-- | Has the same behavior as the @Monad@ instance for @Maybe@. See also:
-- 'bindMaybeTable'.
instance Monad MaybeTable where
  (>>=) = (>>-)


instance AltTable MaybeTable where
  ma@(MaybeTable tag a) <|>: MaybeTable tag' b = MaybeTable
    { tag = boolExpr tag tag' condition
    , just = bool a b condition
    }
    where
      condition = isNothingTable ma


instance AlternativeTable MaybeTable where
  emptyTable = nothingTable


instance (Table Expr a, Semigroup a) => Semigroup (MaybeTable a) where
  ma <> mb = maybeTable mb (\a -> maybeTable ma (justTable . (a <>)) mb) ma


instance (Table Expr a, Semigroup a) => Monoid (MaybeTable a) where
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

  {-# INLINABLE fromColumns1 #-}
  {-# INLINABLE toColumns1 #-}


instance
  ( Table context a
  , Labelable context, Nullifiable context
  ) => Table context (MaybeTable a)
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


-- | Check if a @MaybeTable@ is absent of any row.. Like 'isNothing'.
isNothingTable :: MaybeTable a -> Expr Bool
isNothingTable (MaybeTable tag _) = isNull tag


-- | Check if a @MaybeTable@ contains a row. Like 'isJust'.
isJustTable :: MaybeTable a -> Expr Bool
isJustTable (MaybeTable tag _) = isNonNull tag


-- | Perform case analysis on a 'MaybeTable'. Like 'maybe'.
maybeTable :: Table Expr b => b -> (a -> b) -> MaybeTable a -> b
maybeTable b f ma@(MaybeTable _ a) = bool (f a) b (isNothingTable ma)


-- | The null table. Like 'Nothing'.
nothingTable :: Table Expr a => MaybeTable a
nothingTable = MaybeTable null undefined


-- | Lift any table into 'MaybeTable'. Like 'Just'. Note you can also use
-- 'pure'.
justTable :: a -> MaybeTable a
justTable = MaybeTable (nullify (litExpr IsJust))


-- | Project a single expression out of a 'MaybeTable'. You can think of this
-- operator like the '$' operator, but it also has the ability to return
-- @null@.
--
-- >>> select c $ fmap (fst $?) (optional (values [lit (True, False)]))
-- [Just True]
($?) :: forall a b. Sql DBType b
  => (a -> Expr b) -> MaybeTable a -> Expr (Nullify b)
f $? ma@(MaybeTable _ a) = case nullabilization @b of
  Nullable -> boolExpr (f a) null (isNothingTable ma)
  NonNullable -> boolExpr (nullify (f a)) null (isNothingTable ma)
infixl 4 $?
