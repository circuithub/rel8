{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
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
  , nameMaybeTable
  )
where

-- base
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude hiding ( null, undefined )

-- categories
import qualified Control.Categorical.Functor as Cat

-- rel8
import Rel8.Category.Projection ( Projection( Projection ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( isNull, isNonNull, null, nullify )
import Rel8.Schema.Context.Nullify
  ( Nullifiable, ConstrainTag
  , HNullifiable, HConstrainTag
  , hencodeTag, hdecodeTag
  , hnullifier, hunnullifier
  )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.Nullify ( hnullify, hproject, hunnullify )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Null ( Nullify, Nullity( Null, NotNull ), Sql, nullable )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
import Rel8.Table.Tag ( Tag(..), fromExpr, fromName )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type ( DBType )
import Rel8.Type.Tag ( MaybeTag )

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
  { tag :: Tag "isJust" (Maybe MaybeTag)
  , just :: a
  }
  deriving stock Functor


instance Cat.Functor MaybeTable Projection Projection where
  fmap (Projection f) = Projection $ \HMaybeTable {..} ->
    HMaybeTable {hjust = hlabel (hproject f (hunlabel hjust)), ..}


instance Cat.Endofunctor MaybeTable Projection


instance Apply MaybeTable where
  MaybeTable tag f <.> MaybeTable tag' a = MaybeTable (tag <> tag') (f a)


-- | Has the same behavior as the @Applicative@ instance for @Maybe@. See also:
-- 'Rel8.traverseMaybeTable'.
instance Applicative MaybeTable where
  (<*>) = (<.>)
  pure = justTable


instance Bind MaybeTable where
  MaybeTable tag a >>- f = case f a of
    MaybeTable tag' b -> MaybeTable (tag <> tag') b


-- | Has the same behavior as the @Monad@ instance for @Maybe@.
instance Monad MaybeTable where
  (>>=) = (>>-)


instance AltTable MaybeTable where
  ma@(MaybeTable tag a) <|>: MaybeTable tag' b = MaybeTable
    { tag = (tag <> tag')
        { expr = boolExpr (expr tag) (expr tag') condition
        }
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


instance
  ( Table context a
  , Nullifiable context
  , ConstrainTag context MaybeTag
  ) => Table context (MaybeTable a)
 where
  type Columns (MaybeTable a) = HMaybeTable (Columns a)
  type Context (MaybeTable a) = Context a

  toColumns = toColumns1 toColumns
  fromColumns = fromColumns1 fromColumns
  reify = fmap fmap reify
  unreify = fmap fmap unreify


instance
  ( Nullifiable from, ConstrainTag from MaybeTag
  , Nullifiable to, ConstrainTag to MaybeTag
  , Recontextualize from to a b
  )
  => Recontextualize from to (MaybeTable a) (MaybeTable b)


instance EqTable a => EqTable (MaybeTable a) where
  eqTable = toColumns1 id (justTable (eqTable @a))


instance OrdTable a => OrdTable (MaybeTable a) where
  ordTable = toColumns1 id (justTable (ordTable @a))


type instance FromExprs (MaybeTable a) = Maybe (FromExprs a)


instance ToExprs exprs a => ToExprs (MaybeTable exprs) (Maybe a) where
  fromResult = fmap (fromResult @exprs) . fromColumns
  toResult = toColumns . fmap (toResult @exprs)


-- | Check if a @MaybeTable@ is absent of any row. Like 'Data.Maybe.isNothing'.
isNothingTable :: MaybeTable a -> Expr Bool
isNothingTable (MaybeTable tag _) = isNull (expr tag)


-- | Check if a @MaybeTable@ contains a row. Like 'Data.Maybe.isJust'.
isJustTable :: MaybeTable a -> Expr Bool
isJustTable (MaybeTable tag _) = isNonNull (expr tag)


-- | Perform case analysis on a 'MaybeTable'. Like 'maybe'.
maybeTable :: Table Expr b => b -> (a -> b) -> MaybeTable a -> b
maybeTable b f ma@(MaybeTable _ a) = bool (f a) b (isNothingTable ma)
{-# INLINABLE maybeTable #-}


-- | The null table. Like 'Nothing'.
nothingTable :: Table Expr a => MaybeTable a
nothingTable = MaybeTable (fromExpr null) undefined


-- | Lift any table into 'MaybeTable'. Like 'Just'. Note you can also use
-- 'pure'.
justTable :: a -> MaybeTable a
justTable = MaybeTable (fromExpr mempty)


-- | Project a single expression out of a 'MaybeTable'. You can think of this
-- operator like the '$' operator, but it also has the ability to return
-- @null@.
($?) :: forall a b. Sql DBType b
  => (a -> Expr b) -> MaybeTable a -> Expr (Nullify b)
f $? ma@(MaybeTable _ a) = case nullable @b of
  Null -> boolExpr (f a) null (isNothingTable ma)
  NotNull -> boolExpr (nullify (f a)) null (isNothingTable ma)
infixl 4 $?


-- | Construct a 'MaybeTable' in the 'Name' context. This can be useful if you
-- have a 'MaybeTable' that you are storing in a table and need to construct a
-- 'TableSchema'.
nameMaybeTable
  :: Name (Maybe MaybeTag)
     -- ^ The name of the column to track whether a row is a 'justTable' or
     -- 'nothingTable'.
  -> a
     -- ^ Names of the columns in @a@.
  -> MaybeTable a
nameMaybeTable = MaybeTable . fromName


toColumns1 ::
  ( HTable t
  , HConstrainTag context MaybeTag
  , HNullifiable context
  )
  => (a -> t context)
  -> MaybeTable a
  -> HMaybeTable t context
toColumns1 f MaybeTable {tag, just} = HMaybeTable
  { htag
  , hjust = hlabel $ hnullify (hnullifier tag isNonNull) $ f just
  }
  where
    htag = hlabel (HType (hencodeTag tag))


fromColumns1 ::
  ( HTable t
  , HConstrainTag context MaybeTag
  , HNullifiable context
  )
  => (t context -> a)
  -> HMaybeTable t context
  -> MaybeTable a
fromColumns1 f HMaybeTable {htag, hjust} = MaybeTable
  { tag
  , just = f $ runIdentity $
      hunnullify (\a -> pure . hunnullifier a) (hunlabel hjust)
  }
  where
    tag = hdecodeTag (unHIdentity (hunlabel htag))
