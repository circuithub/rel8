{-# language FlexibleContexts #-}

module Rel8.Query.Either
  ( keepLeftTable
  , keepRightTable
  , bindEitherTable
  , bitraverseEitherTable
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.Set ( unionAll )
import Rel8.Table ( Table )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Either
  ( EitherTable( EitherTable )
  , leftTable, rightTable
  , isLeftTable, isRightTable
  )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ), isJustTable )


keepLeftTable :: EitherTable a b -> Query a
keepLeftTable e@(EitherTable _ a _) = do
  where_ $ isLeftTable e
  pure a


keepRightTable :: EitherTable a b -> Query b
keepRightTable e@(EitherTable _ _ b) = do
  where_ $ isRightTable e
  pure b


bindEitherTable :: (Table Expr a, Monad m)
  => (i -> m (EitherTable a b)) -> EitherTable a i -> m (EitherTable a b)
bindEitherTable query e@(EitherTable input a i) = do
  EitherTable output a' b <- query i
  pure $ EitherTable (input <> output) (bool a a' (isRightTable e)) b


bitraverseEitherTable :: (Table Expr c, Table Expr d)
  => (a -> Query c)
  -> (b -> Query d)
  -> EitherTable a b
  -> Query (EitherTable c d)
bitraverseEitherTable f g e = traverseLeftTable `unionAll` traverseRightTable
  where
    traverseLeftTable = do
      mc@(MaybeTable _ c) <- optional (f =<< keepLeftTable e)
      where_ $ isJustTable mc ==. isLeftTable e
      pure $ leftTable c
    traverseRightTable = do
      md@(MaybeTable _ d) <- optional (g =<< keepRightTable e)
      where_ $ isJustTable md ==. isRightTable e
      pure $ rightTable d
