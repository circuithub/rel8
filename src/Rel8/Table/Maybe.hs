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
  , aggregateMaybeTable
  , nameMaybeTable
  )
where

-- base
import Data.Functor ( ($>) )
import Data.Kind ( Type )
import Data.Maybe ( fromMaybe, isJust )
import Prelude hiding ( null, undefined )

-- categories
import qualified Control.Categorical.Functor as Cat

-- comonad
import Control.Comonad ( extract )

-- rel8
import Rel8.Aggregate ( Aggregate( A ) )
import Rel8.Category.Projection ( Projection( Projection ) )
import Rel8.Expr ( Expr(E ) )
import Rel8.Expr.Aggregate ( groupByExpr )
import Rel8.Expr.Bool ( boolExpr )
import Rel8.Expr.Null ( isNull, isNonNull, null, nullify )
import Rel8.Kind.Context ( Reifiable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.Nullify ( hproject )
import Rel8.Schema.Name ( Name( N ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ), Sql, nullable )
import qualified Rel8.Schema.Null as N
import Rel8.Schema.Result ( Result( R ) )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Nullify ( Nullify, aggregateNullify, guard )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( ToExprs )
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
type MaybeTable :: K.Context -> Type -> Type
data MaybeTable context a = MaybeTable
  { tag :: context ('Spec (Maybe MaybeTag))
  , just :: Nullify context a
  }
  deriving stock Functor


instance Cat.Functor (MaybeTable context) Projection Projection where
  fmap (Projection f) = Projection $ \HMaybeTable {..} ->
   HMaybeTable {hjust = hlabel (hproject f (hunlabel hjust)), ..}


instance Cat.Endofunctor (MaybeTable context) Projection


instance context ~ Expr => Apply (MaybeTable context) where
  MaybeTable (E tag) f <.> MaybeTable (E tag') a =
    MaybeTable (E (tag <> tag')) (f <.> a)


-- | Has the same behavior as the @Applicative@ instance for @Maybe@. See also:
-- 'Rel8.traverseMaybeTable'.
instance context ~ Expr => Applicative (MaybeTable context) where
  (<*>) = (<.>)
  pure = justTable


instance context ~ Expr => Bind (MaybeTable context) where
  MaybeTable (E tag) a >>- f = case f (extract a) of
    MaybeTable (E tag') b -> MaybeTable (E (tag <> tag')) b


-- | Has the same behavior as the @Monad@ instance for @Maybe@.
instance context ~ Expr => Monad (MaybeTable context) where
  (>>=) = (>>-)


instance context ~ Expr => AltTable (MaybeTable context) where
  ma <|>: mb = bool ma mb (isNothingTable ma)


instance context ~ Expr => AlternativeTable (MaybeTable context) where
  emptyTable = nothingTable


instance (context ~ Expr, Table Expr a, Semigroup a) =>
  Semigroup (MaybeTable context a)
 where
  ma <> mb = maybeTable mb (\a -> maybeTable ma (justTable . (a <>)) mb) ma


instance (context ~ Expr, Table Expr a, Semigroup a) =>
  Monoid (MaybeTable context a)
 where
  mempty = nothingTable


instance (Table context a, Reifiable context, context ~ context') =>
  Table context' (MaybeTable context a)
 where
  type Columns (MaybeTable context a) = HMaybeTable (Columns a)
  type Context (MaybeTable context a) = Context a
  type FromExprs (MaybeTable context a) = Maybe (FromExprs a)

  toColumns MaybeTable {tag, just} = HMaybeTable
    { htag = hlabel $ HIdentity tag
    , hjust = hlabel $ guard tag isJust isNonNull $ toColumns just
    }

  fromColumns HMaybeTable {htag, hjust} = MaybeTable
    { tag = unHIdentity $ hunlabel htag
    , just = fromColumns $ hunlabel hjust
    }

  toResult ma = HMaybeTable
    { htag = hlabel (HIdentity (R (IsJust <$ ma)))
    , hjust = hlabel (toResult @_ @(Nullify context a) ma)
    }

  fromResult HMaybeTable {htag, hjust} = case hunlabel htag of
    HType (R tag) -> tag $>
      fromMaybe err (fromResult @_ @(Nullify context a) (hunlabel hjust))
    where
      err = error "Maybe.fromColumns: mismatch between tag and data"


instance
  ( Recontextualize from to a b
  , Reifiable from, from ~ from'
  , Reifiable to, to ~ to'
  )
  => Recontextualize from to (MaybeTable from' a) (MaybeTable to' b)


instance (EqTable a, context ~ Expr) => EqTable (MaybeTable context a) where
  eqTable = HMaybeTable
    { htag = hlabel (HType Dict)
    , hjust = hlabel (eqTable @(Nullify context a))
    }


instance (OrdTable a, context ~ Expr) => OrdTable (MaybeTable context a) where
  ordTable = HMaybeTable
    { htag = hlabel (HType Dict)
    , hjust = hlabel (ordTable @(Nullify context a))
    }


instance (ToExprs exprs a, context ~ Expr) =>
  ToExprs (MaybeTable context exprs) (Maybe a)


-- | Check if a @MaybeTable@ is absent of any row. Like 'Data.Maybe.isNothing'.
isNothingTable :: MaybeTable Expr a -> Expr Bool
isNothingTable (MaybeTable (E tag) _) = isNull tag


-- | Check if a @MaybeTable@ contains a row. Like 'Data.Maybe.isJust'.
isJustTable :: MaybeTable Expr a -> Expr Bool
isJustTable (MaybeTable (E tag) _) = isNonNull tag


-- | Perform case analysis on a 'MaybeTable'. Like 'maybe'.
maybeTable :: Table Expr b => b -> (a -> b) -> MaybeTable Expr a -> b
maybeTable b f ma@(MaybeTable _ a) = bool (f (extract a)) b (isNothingTable ma)
{-# INLINABLE maybeTable #-}


-- | The null table. Like 'Nothing'.
nothingTable :: Table Expr a => MaybeTable Expr a
nothingTable = MaybeTable (E null) (pure undefined)


-- | Lift any table into 'MaybeTable'. Like 'Just'. Note you can also use
-- 'pure'.
justTable :: a -> MaybeTable Expr a
justTable = MaybeTable (E mempty) . pure


-- | Project a single expression out of a 'MaybeTable'. You can think of this
-- operator like the '$' operator, but it also has the ability to return
-- @null@.
($?) :: forall a b. Sql DBType b
  => (a -> Expr b) -> MaybeTable Expr a -> Expr (N.Nullify b)
f $? ma@(MaybeTable _ a) = case nullable @b of
  Null -> boolExpr (f (extract a)) null (isNothingTable ma)
  NotNull -> boolExpr (nullify (f (extract a))) null (isNothingTable ma)
infixl 4 $?


-- | Lift an aggregating function to operate on a 'MaybeTable'.
-- @nothingTable@s and @justTable@s are grouped separately.
aggregateMaybeTable :: ()
  => (exprs -> aggregates)
  -> MaybeTable Expr exprs
  -> MaybeTable Aggregate aggregates
aggregateMaybeTable f (MaybeTable (E tag) a) =
  MaybeTable (A (groupByExpr tag)) (aggregateNullify f a)


-- | Construct a 'MaybeTable' in the 'Name' context. This can be useful if you
-- have a 'MaybeTable' that you are storing in a table and need to construct a
-- 'TableSchema'.
nameMaybeTable
  :: Name (Maybe MaybeTag)
     -- ^ The name of the column to track whether a row is a 'justTable' or
     -- 'nothingTable'.
  -> a
     -- ^ Names of the columns in @a@.
  -> MaybeTable Name a
nameMaybeTable tag = MaybeTable (N tag) . pure
