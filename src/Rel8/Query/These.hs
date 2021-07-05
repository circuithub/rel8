{-# language FlexibleContexts #-}
{-# language GADTs #-}

module Rel8.Query.These
  ( alignBy
  , keepHereTable, loseHereTable
  , keepThereTable, loseThereTable
  , keepThisTable, loseThisTable
  , keepThatTable, loseThatTable
  , keepThoseTable, loseThoseTable
  , bitraverseTheseTable
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Expr ( Col( E ), Expr )
import Rel8.Expr.Bool ( boolExpr, not_ )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Opaleye ( toPrimExpr, traversePrimExpr )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.Opaleye ( zipOpaleyeWith )
import Rel8.Table.Either ( EitherTable( EitherTable ) )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ), isJustTable )
import Rel8.Table.These
  ( TheseTable( TheseTable, here, there )
  , hasHereTable, hasThereTable
  , isThisTable, isThatTable, isThoseTable
  )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ) )


-- | Corresponds to a @FULL OUTER JOIN@ between two queries.
alignBy :: ()
  => (a -> b -> Expr Bool)
  -> Query a -> Query b -> Query (TheseTable Expr a b)
alignBy condition = zipOpaleyeWith $ \left right -> Opaleye.QueryArr $ \i -> case i of
  (_, tag) -> (tab, join', tag''')
    where
      (ma, left', tag') = Opaleye.runSimpleQueryArr (pure <$> left) ((), tag)
      (mb, right', tag'') = Opaleye.runSimpleQueryArr (pure <$> right) ((), tag')
      MaybeTable (E hasHere) a = ma
      MaybeTable (E hasThere) b = mb
      (hasHere', lbindings) = Opaleye.run $ do
        traversePrimExpr (Opaleye.extractAttr "hasHere" tag'') hasHere
      (hasThere', rbindings) = Opaleye.run $ do
        traversePrimExpr (Opaleye.extractAttr "hasThere" tag'') hasThere
      tag''' = Opaleye.next tag''
      join lateral = Opaleye.Join Opaleye.FullJoin on left'' right''
        where
          on = toPrimExpr $ condition a b
          left'' = (lateral, Opaleye.Rebind True lbindings left')
          right'' = (lateral, Opaleye.Rebind True rbindings right')
      ma' = MaybeTable (E hasHere') a
      mb' = MaybeTable (E hasThere') b
      tab = TheseTable {here = ma', there = mb'}
      join' lateral input = Opaleye.times lateral input (join lateral)


keepHereTable :: TheseTable Expr a b -> Query (a, MaybeTable Expr b)
keepHereTable = loseThatTable


loseHereTable :: TheseTable Expr a b -> Query b
loseHereTable = keepThatTable


keepThereTable :: TheseTable Expr a b -> Query (MaybeTable Expr a, b)
keepThereTable = loseThisTable


loseThereTable :: TheseTable Expr a b -> Query a
loseThereTable = keepThisTable


keepThisTable :: TheseTable Expr a b -> Query a
keepThisTable t@(TheseTable (MaybeTable _ a) _) = do
  where_ $ isThisTable t
  pure a


loseThisTable :: TheseTable Expr a b -> Query (MaybeTable Expr a, b)
loseThisTable t@(TheseTable ma (MaybeTable _ b)) = do
  where_ $ not_ $ isThisTable t
  pure (ma, b)


keepThatTable :: TheseTable Expr a b -> Query b
keepThatTable t@(TheseTable _ (MaybeTable _ b)) = do
  where_ $ isThatTable t
  pure b


loseThatTable :: TheseTable Expr a b -> Query (a, MaybeTable Expr b)
loseThatTable t@(TheseTable (MaybeTable _ a) mb) = do
  where_ $ not_ $ isThatTable t
  pure (a, mb)


keepThoseTable :: TheseTable Expr a b -> Query (a, b)
keepThoseTable t@(TheseTable (MaybeTable _ a) (MaybeTable _ b)) = do
  where_ $ isThoseTable t
  pure (a, b)


loseThoseTable :: TheseTable Expr a b -> Query (EitherTable Expr a b)
loseThoseTable t@(TheseTable (MaybeTable _ a) (MaybeTable _ b)) = do
  where_ $ not_ $ isThoseTable t
  pure $ EitherTable (E tag) a b
  where
    tag = boolExpr (litExpr IsLeft) (litExpr IsRight) (isThatTable t)


bitraverseTheseTable :: ()
  => (a -> Query c)
  -> (b -> Query d)
  -> TheseTable Expr a b
  -> Query (TheseTable Expr c d)
bitraverseTheseTable f g t = do
  mc <- optional (f . fst =<< keepHereTable t)
  md <- optional (g . snd =<< keepThereTable t)
  where_ $ isJustTable mc ==. hasHereTable t
  where_ $ isJustTable md ==. hasThereTable t
  pure $ TheseTable mc md
