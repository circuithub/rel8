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
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Table.These
  ( TheseTable(..)
  , theseTable, thisTable, thatTable, thoseTable
  , isThisTable, isThatTable, isThoseTable
  , hasHereTable, hasThereTable
  , justHereTable, justThereTable
  , nameTheseTable
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude hiding ( undefined )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (&&.), not_ )
import Rel8.Expr.Null ( isNonNull )
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
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import Rel8.Schema.HTable.These ( HTheseTable(..) )
import Rel8.Schema.Name ( Name )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Maybe
  ( MaybeTable(..)
  , maybeTable, justTable, nothingTable
  , isJustTable
  , nameMaybeTable
  )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
import Rel8.Table.Tag ( Tag(..) )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type.Tag ( MaybeTag )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )

-- these
import Data.These ( These )


-- | @TheseTable a b@ is a Rel8 table that contains either the table @a@, the
-- table @b@, or both tables @a@ and @b@. You can construct @TheseTable@s using
-- 'thisTable', 'thatTable' and 'thoseTable'. @TheseTable@s can be
-- eliminated/pattern matched using 'theseTable'.
--
-- @TheseTable@ is operationally the same as Haskell's 'These' type, but
-- adapted to work with Rel8.
type TheseTable :: Type -> Type -> Type
data TheseTable a b = TheseTable
  { here :: MaybeTable a
  , there :: MaybeTable b
  }
  deriving stock Functor


instance Bifunctor TheseTable where
  bimap f g (TheseTable a b) = TheseTable (fmap f a) (fmap g b)


instance (Table Expr a, Semigroup a) => Apply (TheseTable a) where
  fs <.> as = TheseTable
    { here = here fs <> here as
    , there = there fs <.> there as
    }


instance (Table Expr a, Semigroup a) => Applicative (TheseTable a)
 where
  pure = thatTable
  (<*>) = (<.>)


instance (Table Expr a, Semigroup a) => Bind (TheseTable a) where
  TheseTable here1 ma >>- f = case ma >>- f' of
    mtb -> TheseTable
      { here = maybeTable here1 ((here1 <>) . fst) mtb
      , there = snd <$> mtb
      }
    where
      f' a = case f a of
        TheseTable here2 mb -> (here2,) <$> mb


instance (Table Expr a, Semigroup a) => Monad (TheseTable a) where
  (>>=) = (>>-)


instance (Table Expr a, Table Expr b, Semigroup a, Semigroup b) =>
  Semigroup (TheseTable a b)
 where
  a <> b = TheseTable
    { here = here a <> here b
    , there = there a <> there b
    }


instance
  ( Table context a, Table context b
  , Labelable context, Nullifiable context, ConstrainTag context MaybeTag
  ) => Table context (TheseTable a b)
 where
  type Columns (TheseTable a b) = HTheseTable (Columns a) (Columns b)
  type Context (TheseTable a b) = Context a

  toColumns = toColumns2 toColumns toColumns
  fromColumns = fromColumns2 fromColumns fromColumns
  reify = liftA2 bimap reify reify
  unreify = liftA2 bimap unreify unreify


instance
  ( Labelable from, Nullifiable from, ConstrainTag from MaybeTag
  , Labelable to, Nullifiable to, ConstrainTag to MaybeTag
  , Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  ) =>
  Recontextualize from to (TheseTable a1 a2) (TheseTable b1 b2)


instance (EqTable a, EqTable b) => EqTable (TheseTable a b) where
  eqTable = toColumns2 id id (thoseTable (eqTable @a) (eqTable @b))


instance (OrdTable a, OrdTable b) => OrdTable (TheseTable a b) where
  ordTable = toColumns2 id id (thoseTable (ordTable @a) (ordTable @b))


type instance FromExprs (TheseTable a b) = These (FromExprs a) (FromExprs b)


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ TheseTable exprs1 exprs2) =>
  ToExprs x (These a b)
 where
  fromResult =
    bimap (fromResult @exprs1) (fromResult @exprs2) .
    fromColumns
  toResult =
    toColumns .
    bimap (toResult @exprs1) (toResult @exprs2)


toHereTag :: Tag "isJust" a -> Tag "hasHere" a
toHereTag Tag {..} = Tag {..}


toThereTag :: Tag "isJust" a -> Tag "hasThere" a
toThereTag Tag {..} = Tag {..}


-- | Test if a 'TheseTable' was constructed with 'thisTable'.
--
-- Corresponds to 'Data.These.Combinators.isThis'.
isThisTable :: TheseTable a b -> Expr Bool
isThisTable a = hasHereTable a &&. not_ (hasThereTable a)


-- | Test if a 'TheseTable' was constructed with 'thatTable'.
--
-- Corresponds to 'Data.These.Combinators.isThat'.
isThatTable :: TheseTable a b -> Expr Bool
isThatTable a = not_ (hasHereTable a) &&. hasThereTable a


-- | Test if a 'TheseTable' was constructed with 'thoseTable'.
--
-- Corresponds to 'Data.These.Combinators.isThese'.
isThoseTable :: TheseTable a b -> Expr Bool
isThoseTable a = hasHereTable a &&. hasThereTable a


-- | Test if the @a@ side of @TheseTable a b@ is present.
--
-- Corresponds to 'Data.These.Combinators.hasHere'.
hasHereTable :: TheseTable a b -> Expr Bool
hasHereTable TheseTable {here} = isJustTable here


-- | Test if the @b@ table of @TheseTable a b@ is present.
--
-- Corresponds to 'Data.These.Combinators.hasThere'.
hasThereTable :: TheseTable a b -> Expr Bool
hasThereTable TheseTable {there} = isJustTable there


-- | Attempt to project out the @a@ table of a @TheseTable a b@.
--
-- Corresponds to 'Data.These.Combinators.justHere'.
justHereTable :: TheseTable a b -> MaybeTable a
justHereTable = here


-- | Attempt to project out the @b@ table of a @TheseTable a b@.
--
-- Corresponds to 'Data.These.Combinators.justThere'.
justThereTable :: TheseTable a b -> MaybeTable b
justThereTable = there


-- | Construct a @TheseTable@. Corresponds to 'This'.
thisTable :: Table Expr b => a -> TheseTable a b
thisTable a = TheseTable (justTable a) nothingTable


-- | Construct a @TheseTable@. Corresponds to 'That'.
thatTable :: Table Expr a => b -> TheseTable a b
thatTable b = TheseTable nothingTable (justTable b)


-- | Construct a @TheseTable@. Corresponds to 'These'.
thoseTable :: a -> b -> TheseTable a b
thoseTable a b = TheseTable (justTable a) (justTable b)


-- | Pattern match on a 'TheseTable'. Corresponds to 'these'.
theseTable :: Table Expr c
  => (a -> c) -> (b -> c) -> (a -> b -> c) -> TheseTable a b -> c
theseTable f g h TheseTable {here, there} =
  maybeTable
    (maybeTable undefined f here)
    (\b -> maybeTable (g b) (`h` b) here)
    there


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
  -> TheseTable a b
nameTheseTable here there a b =
  TheseTable
    { here = nameMaybeTable here a
    , there = nameMaybeTable there b
    }


toColumns2 ::
  ( HTable t
  , HTable u
  , HConstrainTag context MaybeTag
  , HLabelable context
  , HNullifiable context
  )
  => (a -> t context)
  -> (b -> u context)
  -> TheseTable a b
  -> HTheseTable t u context
toColumns2 f g TheseTable {here, there} = HTheseTable
  { hhereTag = HIdentity $ hencodeTag (toHereTag (tag here))
  , hhere =
      hlabel hlabeler $ hnullify (hnullifier (tag here) isNonNull) $ f (just here)
  , hthereTag = HIdentity $ hencodeTag (toThereTag (tag there))
  , hthere =
      hlabel hlabeler $ hnullify (hnullifier (tag there) isNonNull) $ g (just there)
  }


fromColumns2 ::
  ( HTable t
  , HTable u
  , HConstrainTag context MaybeTag
  , HLabelable context
  , HNullifiable context
  )
  => (t context -> a)
  -> (u context -> b)
  -> HTheseTable t u context
  -> TheseTable a b
fromColumns2 f g HTheseTable {hhereTag, hhere, hthereTag, hthere} = TheseTable
  { here =
      let
        tag = hdecodeTag $ unHIdentity hhereTag
      in
        MaybeTable
          { tag
          , just = f $
              runIdentity $
              hunnullify (\a -> pure . hunnullifier a) $
              hunlabel hunlabeler
              hhere
          }
  , there =
      let
        tag = hdecodeTag $ unHIdentity hthereTag
      in
        MaybeTable
          { tag
          , just = g $
              runIdentity $
              hunnullify (\a -> pure . hunnullifier a) $
              hunlabel hunlabeler
              hthere
          }
  }
