{-# language FlexibleContexts #-}

module Rel8.Query.These
  ( alignBy
  , keepHereTable, loseHereTable
  , keepThereTable, loseThereTable
  , keepThisTable, loseThisTable
  , keepThatTable, loseThatTable
  , keepThoseTable, loseThoseTable
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Join as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( not_ )
import Rel8.Expr.Opaleye ( exprToColumn )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )
import Rel8.Query.Opaleye ( zipOpaleyeWith )
import Rel8.Schema.Context ( DB )
import Rel8.Table ( Table )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Either ( EitherTable( EitherTable ) )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ) )
import Rel8.Table.Opaleye ( unpackspec )
import Rel8.Table.Serialize ( lit )
import Rel8.Table.These
  ( TheseTable( TheseTable )
  , isThisTable, isThatTable, isThoseTable
  )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ) )


alignBy :: (Table DB a, Table DB b)
  => (a -> b -> Expr nullability Bool)
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
  pure $ EitherTable (bool (lit IsLeft) (lit IsRight) (isThatTable t)) a b
