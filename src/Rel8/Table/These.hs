{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Table.These
  ( TheseTable(..)
  , theseTable, thisTable, thatTable, thoseTable
  , isThisTable, isThatTable, isThoseTable
  , hasHereTable, hasThereTable
  , justHereTable, justThereTable
  , alignMaybeTable
  , aggregateTheseTable
  , nameTheseTable
  )
where

-- base
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Kind ( Type )
import Data.Maybe ( isJust )
import Prelude hiding ( null, undefined )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (&&.), (||.), boolExpr, not_ )
import Rel8.Expr.Null ( null, isNonNull )
import Rel8.Kind.Context ( Reifiable )
import Rel8.Schema.Context.Nullify ( Nullifiable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.Label ( hlabel, hrelabel, hunlabel )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Maybe ( HMaybeTable(..) )
import Rel8.Schema.HTable.These ( HTheseTable(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Maybe
  ( MaybeTable(..)
  , maybeTable, justTable, nothingTable
  , isJustTable
  , aggregateMaybeTable
  , nameMaybeTable
  )
import Rel8.Table.Nullify ( Nullify, guard )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Projection ( Biprojectable, Projectable, biproject, project )
import Rel8.Table.Serialize ( ToExprs )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type.Tag ( MaybeTag )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )

-- these
import Data.These ( These( This, That, These ) )
import Data.These.Combinators ( justHere, justThere )


-- | @TheseTable a b@ is a Rel8 table that contains either the table @a@, the
-- table @b@, or both tables @a@ and @b@. You can construct @TheseTable@s using
-- 'thisTable', 'thatTable' and 'thoseTable'. @TheseTable@s can be
-- eliminated/pattern matched using 'theseTable'.
--
-- @TheseTable@ is operationally the same as Haskell's 'These' type, but
-- adapted to work with Rel8.
type TheseTable :: K.Context -> Type -> Type -> Type
data TheseTable context a b = TheseTable
  { here :: MaybeTable context a
  , there :: MaybeTable context b
  }
  deriving stock Functor


instance Biprojectable (TheseTable context) where
  biproject f g (TheseTable a b) = TheseTable (project f a) (project g b)


instance Nullifiable context => Bifunctor (TheseTable context) where
  bimap f g (TheseTable a b) = TheseTable (fmap f a) (fmap g b)


instance Projectable (TheseTable context a) where
  project f (TheseTable a b) = TheseTable a (project f b)


instance (context ~ Expr, Table Expr a, Semigroup a) =>
  Apply (TheseTable context a)
 where
  fs <.> as = TheseTable
    { here = here fs <> here as
    , there = there fs <.> there as
    }


instance (context ~ Expr, Table Expr a, Semigroup a) =>
  Applicative (TheseTable context a)
 where
  pure = thatTable
  (<*>) = (<.>)


instance (context ~ Expr, Table Expr a, Semigroup a) =>
  Bind (TheseTable context a)
 where
  TheseTable here1 ma >>- f = case ma >>- f' of
    mtb -> TheseTable
      { here = maybeTable here1 ((here1 <>) . fst) mtb
      , there = snd <$> mtb
      }
    where
      f' a = case f a of
        TheseTable here2 mb -> (here2,) <$> mb


instance (context ~ Expr, Table Expr a, Semigroup a) =>
  Monad (TheseTable context a)
 where
  (>>=) = (>>-)


instance (context ~ Expr, Table Expr a, Table Expr b, Semigroup a, Semigroup b) =>
  Semigroup (TheseTable context a b)
 where
  a <> b = TheseTable
    { here = here a <> here b
    , there = there a <> there b
    }


instance
  ( Table context a, Table context b
  , Reifiable context, context ~ context'
  )
  => Table context' (TheseTable context a b)
 where
  type Columns (TheseTable context a b) = HTheseTable (Columns a) (Columns b)
  type Context (TheseTable context a b) = Context a
  type FromExprs (TheseTable context a b) =
    These (FromExprs a) (FromExprs b)
  type Transpose to (TheseTable context a b) =
    TheseTable to (Transpose to a) (Transpose to b)

  toColumns TheseTable {here, there} = HTheseTable
    { hhereTag = hlabel $ HIdentity $ tag here
    , hhere =
        hlabel $ guard (tag here) isJust isNonNull $ toColumns $ just here
    , hthereTag = hlabel $ HIdentity $ tag there
    , hthere =
        hlabel $ guard (tag there) isJust isNonNull $ toColumns $ just there
    }

  fromColumns HTheseTable {hhereTag, hhere, hthereTag, hthere} = TheseTable
    { here = MaybeTable
        { tag = unHIdentity $ hunlabel hhereTag
        , just = fromColumns $ hunlabel hhere
        }
    , there = MaybeTable
        { tag = unHIdentity $ hunlabel hthereTag
        , just = fromColumns $ hunlabel hthere
        }
    }

  toResult tables = HTheseTable
    { hhereTag = hrelabel hhereTag
    , hhere = hrelabel hhere
    , hthereTag = hrelabel hthereTag
    , hthere = hrelabel hthere
    }
    where
      HMaybeTable
        { htag = hhereTag
        , hjust = hhere
        } = toResult @_ @(MaybeTable context a) (justHere tables)
      HMaybeTable
        { htag = hthereTag
        , hjust = hthere
        } = toResult @_ @(MaybeTable context b) (justThere tables)

  fromResult HTheseTable {hhereTag, hhere, hthereTag, hthere} =
    case (here, there) of
      (Just a, Nothing) -> This a
      (Nothing, Just b) -> That b
      (Just a, Just b) -> These a b
      _ -> error "These.fromColumns: mismatch between tags and data"
    where
      here = fromResult @_ @(MaybeTable context a) mhere
      there = fromResult @_ @(MaybeTable context b) mthere
      mhere = HMaybeTable
        { htag = hrelabel hhereTag
        , hjust = hrelabel hhere
        }
      mthere = HMaybeTable
        { htag = hrelabel hthereTag
        , hjust = hrelabel hthere
        }


instance (EqTable a, EqTable b, context ~ Expr) =>
  EqTable (TheseTable context a b)
 where
  eqTable = HTheseTable
    { hhereTag = hlabel (HIdentity Dict)
    , hhere = hlabel (eqTable @(Nullify context a))
    , hthereTag = hlabel (HIdentity Dict)
    , hthere = hlabel (eqTable @(Nullify context b))
    }


instance (OrdTable a, OrdTable b, context ~ Expr) =>
  OrdTable (TheseTable context a b)
 where
  ordTable = HTheseTable
    { hhereTag = hlabel (HIdentity Dict)
    , hhere = hlabel (ordTable @(Nullify context a))
    , hthereTag = hlabel (HIdentity Dict)
    , hthere = hlabel (ordTable @(Nullify context b))
    }


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ TheseTable Expr exprs1 exprs2) =>
  ToExprs x (These a b)


-- | Test if a 'TheseTable' was constructed with 'thisTable'.
--
-- Corresponds to 'Data.These.Combinators.isThis'.
isThisTable :: TheseTable Expr a b -> Expr Bool
isThisTable a = hasHereTable a &&. not_ (hasThereTable a)


-- | Test if a 'TheseTable' was constructed with 'thatTable'.
--
-- Corresponds to 'Data.These.Combinators.isThat'.
isThatTable :: TheseTable Expr a b -> Expr Bool
isThatTable a = not_ (hasHereTable a) &&. hasThereTable a


-- | Test if a 'TheseTable' was constructed with 'thoseTable'.
--
-- Corresponds to 'Data.These.Combinators.isThese'.
isThoseTable :: TheseTable Expr a b -> Expr Bool
isThoseTable a = hasHereTable a &&. hasThereTable a


-- | Test if the @a@ side of @TheseTable a b@ is present.
--
-- Corresponds to 'Data.These.Combinators.hasHere'.
hasHereTable :: TheseTable Expr a b -> Expr Bool
hasHereTable TheseTable {here} = isJustTable here


-- | Test if the @b@ table of @TheseTable a b@ is present.
--
-- Corresponds to 'Data.These.Combinators.hasThere'.
hasThereTable :: TheseTable Expr a b -> Expr Bool
hasThereTable TheseTable {there} = isJustTable there


-- | Attempt to project out the @a@ table of a @TheseTable a b@.
--
-- Corresponds to 'Data.These.Combinators.justHere'.
justHereTable :: TheseTable context a b -> MaybeTable context a
justHereTable = here


-- | Attempt to project out the @b@ table of a @TheseTable a b@.
--
-- Corresponds to 'Data.These.Combinators.justThere'.
justThereTable :: TheseTable context a b -> MaybeTable context b
justThereTable = there


-- | Construct a @TheseTable@ from two 'MaybeTable's.
alignMaybeTable :: ()
  => MaybeTable Expr a
  -> MaybeTable Expr b
  -> MaybeTable Expr (TheseTable Expr a b)
alignMaybeTable a b = MaybeTable tag (pure (TheseTable a b))
  where
    tag = boolExpr null mempty (isJustTable a ||. isJustTable b)


-- | Construct a @TheseTable@. Corresponds to 'This'.
thisTable :: Table Expr b => a -> TheseTable Expr a b
thisTable a = TheseTable (justTable a) nothingTable


-- | Construct a @TheseTable@. Corresponds to 'That'.
thatTable :: Table Expr a => b -> TheseTable Expr a b
thatTable b = TheseTable nothingTable (justTable b)


-- | Construct a @TheseTable@. Corresponds to 'These'.
thoseTable :: a -> b -> TheseTable Expr a b
thoseTable a b = TheseTable (justTable a) (justTable b)


-- | Pattern match on a 'TheseTable'. Corresponds to 'these'.
theseTable :: Table Expr c
  => (a -> c) -> (b -> c) -> (a -> b -> c) -> TheseTable Expr a b -> c
theseTable f g h TheseTable {here, there} =
  maybeTable
    (maybeTable undefined f here)
    (\b -> maybeTable (g b) (`h` b) here)
    there


-- | Lift a pair of aggregating functions to operate on an 'TheseTable'.
-- @thisTable@s, @thatTable@s and @thoseTable@s are grouped separately.
aggregateTheseTable :: ()
  => (exprs -> aggregates)
  -> (exprs' -> aggregates')
  -> TheseTable Expr exprs exprs'
  -> TheseTable Aggregate aggregates aggregates'
aggregateTheseTable f g (TheseTable here there) = TheseTable
  { here = aggregateMaybeTable f here
  , there = aggregateMaybeTable g there
  }


-- | Construct a 'TheseTable' in the 'Name' context. This can be useful if you
-- have a 'TheseTable' that you are storing in a table and need to construct a
-- 'TableSchema'.
nameTheseTable :: ()
  => Name (Maybe MaybeTag)
     -- ^ The name of the column to track the presence of the @a@ table.
  -> Name (Maybe MaybeTag)
     -- ^ The name of the column to track the presence of the @b@ table.
  -> a
     -- ^ Names of the columns in the @a@ table.
  -> b
     -- ^ Names of the columns in the @b@ table.
  -> TheseTable Name a b
nameTheseTable here there a b =
  TheseTable
    { here = nameMaybeTable here a
    , there = nameMaybeTable there b
    }
