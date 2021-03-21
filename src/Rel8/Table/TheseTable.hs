{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wno-orphans #-}

module Rel8.Table.TheseTable
  ( TheseTable(..)
  , theseTable
  , thisTable
  , thatTable
  , thoseTable
  , isThisTable
  , isThatTable
  , isThoseTable
  , hasHereTable
  , hasThereTable
  , justHereTable
  , justThereTable
  , keepHereTable
  , loseHereTable
  , keepThereTable
  , loseThereTable
  , keepThoseTable
  , bindTheseTable
  , bitraverseTheseTable
  , alignBy
  , loseThoseTable
  ) where

-- base
import Data.Bifunctor ( Bifunctor, bimap )
import Prelude ( Applicative, Bool, Functor, Maybe( Just, Nothing ), Monad, Semigroup, ($), (.), (<$>), (<*>), (<>), (>>=), error, fmap, fst, pure, snd, uncurry )

-- rel8
import qualified Opaleye.Internal.Join as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (&&.), not_ )
import Rel8.Expr.Opaleye ( exprToColumn )
import Rel8.HTable.HPair ( HPair( HPair ) )
import Rel8.Query ( Query, where_, zipOpaleyeWith )
import Rel8.Serializable ( ExprFor, Serializable, lit, pack, unpack )
import Rel8.Table ( Columns, Table, fromColumns, nullTable, toColumns )
import Rel8.Table.Bool ( bool )
import Rel8.Table.EitherTable ( EitherTable( EitherTable ), EitherTag( IsLeft, IsRight ) )
import Rel8.Table.MaybeTable
  ( MaybeTable( MaybeTable )
  , isJustTable
  , justTable
  , maybeTable
  , nothingTable
  , traverseMaybeTable
  )
import Rel8.Table.Opaleye ( unpackspec )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )

-- these
import Data.These ( These( This, These, That ) )


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


instance (Table Expr a, Table Expr b) => Table Expr (TheseTable a b) where
  type Columns (TheseTable a b) = HPair (Columns (MaybeTable a)) (Columns (MaybeTable b))

  toColumns (TheseTable l r) = HPair (toColumns l) (toColumns r)
  fromColumns (HPair l r) = TheseTable (fromColumns l) (fromColumns r)


instance (a ~ TheseTable d e, Serializable d b, Serializable e c) => ExprFor a (These b c) where
  pack (HPair l r) =
    case (pack @(MaybeTable d) @(Maybe b) l, pack @(MaybeTable e) @(Maybe c) r) of
      (Just x , Nothing) -> This x
      (Just x , Just y ) -> These x y
      (Nothing, Just y ) -> That y
      _                  -> error ""

  unpack = \case
    This x    -> HPair (unpack @(MaybeTable d) @(Maybe b) (Just x)) (unpack @(MaybeTable e) @(Maybe c) Nothing)
    These x y -> HPair (unpack @(MaybeTable d) @(Maybe b) (Just x)) (unpack @(MaybeTable e) @(Maybe c) (Just y))
    That y    -> HPair (unpack @(MaybeTable d) @(Maybe b) Nothing)  (unpack @(MaybeTable e) @(Maybe c) (Just y))


isThisTable :: TheseTable a b -> Expr Bool
isThisTable a = hasHereTable a &&. not_ (hasThereTable a)


isThatTable :: TheseTable a b -> Expr Bool
isThatTable a = not_ (hasHereTable a) &&. hasThereTable a


isThoseTable :: TheseTable a b -> Expr Bool
isThoseTable a = hasHereTable a &&. hasThereTable a


hasHereTable :: TheseTable a b -> Expr Bool
hasHereTable TheseTable {here} = isJustTable here


hasThereTable :: TheseTable a b -> Expr Bool
hasThereTable TheseTable {there} = isJustTable there


justHereTable :: TheseTable a b -> MaybeTable a
justHereTable = here


justThereTable :: TheseTable a b -> MaybeTable b
justThereTable = there


thisTable :: Table Expr b => a -> TheseTable a b
thisTable a = TheseTable (justTable a) nothingTable


thatTable :: Table Expr a => b -> TheseTable a b
thatTable b = TheseTable nothingTable (justTable b)


thoseTable :: a -> b -> TheseTable a b
thoseTable a b = TheseTable (justTable a) (justTable b)


theseTable :: Table Expr c
  => (a -> c) -> (b -> c) -> (a -> b -> c) -> TheseTable a b -> c
theseTable f g h TheseTable {here, there} =
  maybeTable
    (maybeTable nullTable f here)
    (\b -> maybeTable (g b) (`h` b) here)
    there


alignBy :: (Table Expr a, Table Expr b)
  => (a -> b -> Expr Bool)
  -> Query a -> Query b -> Query (TheseTable a b)
alignBy condition as bs =
  uncurry TheseTable <$> zipOpaleyeWith fullOuterJoin as bs
  where
    fullOuterJoin a b =
      Opaleye.joinExplicit unpackspec unpackspec pure pure full a b on
      where
        full = Opaleye.FullJoin
        on = exprToColumn . uncurry condition


keepHereTable :: TheseTable a b -> Query (a, MaybeTable b)
keepHereTable = loseThatTable


loseHereTable :: TheseTable a b -> Query b
loseHereTable = keepThatTable


keepThereTable :: TheseTable a b -> Query (MaybeTable a, b)
keepThereTable = loseThisTable


loseThereTable :: TheseTable a b -> Query a
loseThereTable = keepThisTable


keepThisTable :: TheseTable a b -> Query a
keepThisTable t@(TheseTable (MaybeTable _ a) _) = do
  where_ $ isThisTable t
  pure a


loseThisTable :: TheseTable a b -> Query (MaybeTable a, b)
loseThisTable t@(TheseTable ma (MaybeTable _ b)) = do
  where_ $ not_ $ isThisTable t
  pure (ma, b)


keepThatTable :: TheseTable a b -> Query b
keepThatTable t@(TheseTable _ (MaybeTable _ b)) = do
  where_ $ isThatTable t
  pure b


loseThatTable :: TheseTable a b -> Query (a, MaybeTable b)
loseThatTable t@(TheseTable (MaybeTable _ a) mb) = do
  where_ $ not_ $ isThatTable t
  pure (a, mb)


keepThoseTable :: TheseTable a b -> Query (a, b)
keepThoseTable t@(TheseTable (MaybeTable _ a) (MaybeTable _ b)) = do
  where_ $ isThoseTable t
  pure (a, b)


loseThoseTable :: TheseTable a b -> Query (EitherTable a b)
loseThoseTable t@(TheseTable (MaybeTable _ a) (MaybeTable _ b)) = do
  where_ $ not_ $ isThoseTable t
  pure $ EitherTable tag a b
  where
    tag = bool (lit IsLeft) (lit IsRight) (isThatTable t)


bindTheseTable :: (Table Expr a, Semigroup a, Monad m)
  => (i -> m (TheseTable a b)) -> TheseTable a i -> m (TheseTable a b)
bindTheseTable query (TheseTable here (MaybeTable input i)) = do
  TheseTable here' (MaybeTable output b) <- query i
  pure $ TheseTable (here <> here') (MaybeTable (input <> output) b)


bitraverseTheseTable :: (Table Expr c, Table Expr d)
  => (a -> Query c)
  -> (b -> Query d)
  -> TheseTable a b
  -> Query (TheseTable c d)
bitraverseTheseTable f g (TheseTable here there) =
  bimap fromJustTable fromJustTable <$>
    alignBy (\l r -> isJustTable l &&. isJustTable r)
      (traverseMaybeTable f here)
      (traverseMaybeTable g there)
  where
    fromJustTable (MaybeTable _ a) = a
