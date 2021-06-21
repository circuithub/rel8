{-# language FlexibleContexts #-}

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
import Rel8.Expr ( Expr )
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
import Rel8.Table.Tag ( Tag(..), fromExpr )
import Rel8.Table.These
  ( TheseTable( TheseTable, here, there )
  , hasHereTable, hasThereTable
  , isThisTable, isThatTable, isThoseTable
  )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ) )


-- | Corresponds to a @FULL OUTER JOIN@ between two queries.
alignBy :: ()
  => (a -> b -> Expr Bool)
  -> Query a -> Query b -> Query (TheseTable a b)
alignBy condition = zipOpaleyeWith $ \left right -> Opaleye.QueryArr $ \i -> case i of
  (_, input, tag) -> (tab, join', tag''')
    where
      (ma, left', tag') = Opaleye.runSimpleQueryArr (pure <$> left) ((), tag)
      (mb, right', tag'') = Opaleye.runSimpleQueryArr (pure <$> right) ((), tag')
      MaybeTable Tag {expr = hasHere} a = ma
      MaybeTable Tag {expr = hasThere} b = mb
      (hasHere', lbindings) = Opaleye.run $ do
        traversePrimExpr (Opaleye.extractAttr "hasHere" tag'') hasHere
      (hasThere', rbindings) = Opaleye.run $ do
        traversePrimExpr (Opaleye.extractAttr "hasThere" tag'') hasThere
      tag''' = Opaleye.next tag''
      join = Opaleye.Join Opaleye.FullJoin on lbindings rbindings left' right'
        where
          on = toPrimExpr $ condition a b
      ma' = MaybeTable (fromExpr hasHere') a
      mb' = MaybeTable (fromExpr hasThere') b
      tab = TheseTable {here = ma', there = mb'}
      join' = Opaleye.times input join


-- | Filter 'TheseTable's, keeping only 'thisTable's and 'thoseTable's.
keepHereTable :: TheseTable a b -> Query (a, MaybeTable b)
keepHereTable = loseThatTable


-- | Filter 'TheseTable's, keeping on
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
  pure $ EitherTable result a b
  where
    tag = boolExpr (litExpr IsLeft) (litExpr IsRight) (isThatTable t)
    result = (mempty `asTypeOf` result) {expr = tag}


bitraverseTheseTable :: ()
  => (a -> Query c)
  -> (b -> Query d)
  -> TheseTable a b
  -> Query (TheseTable c d)
bitraverseTheseTable f g t = do
  mc <- optional (f . fst =<< keepHereTable t)
  md <- optional (g . snd =<< keepThereTable t)
  where_ $ isJustTable mc ==. hasHereTable t
  where_ $ isJustTable md ==. hasThereTable t
  pure $ TheseTable mc md
