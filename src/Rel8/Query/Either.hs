{-# language FlexibleContexts #-}

module Rel8.Query.Either
  ( keepLeftTable
  , keepRightTable
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
import Rel8.Table.Either
  ( EitherTable( EitherTable )
  , isLeftTable, isRightTable
  )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ), isJustTable )


-- | Filter 'EitherTable's, keeping only 'leftTable's.
keepLeftTable :: EitherTable Expr a b -> Query a
keepLeftTable e@(EitherTable _ a _) = do
  where_ $ isLeftTable e
  pure a


-- | Filter 'EitherTable's, keeping only 'rightTable's.
keepRightTable :: EitherTable Expr a b -> Query b
keepRightTable e@(EitherTable _ _ b) = do
  where_ $ isRightTable e
  pure b


-- | @bitraverseEitherTable f g x@ will pass all @leftTable@s through @f@ and
-- all @rightTable@s through @g@. The results are then lifted back into
-- @leftTable@ and @rightTable@, respectively. This is similar to 'bitraverse'
-- for 'Either'.
--
-- For example,
--
-- >>> :{
-- select do
--   x <- values (map lit [ Left True, Right (42 :: Int32) ])
--   bitraverseEitherTable (\y -> values [y, not_ y]) (\y -> pure (y * 100)) x
-- :}
-- [ Left True
-- , Left False
-- , Right 4200
-- ]
bitraverseEitherTable :: ()
  => (a -> Query c)
  -> (b -> Query d)
  -> EitherTable Expr a b
  -> Query (EitherTable Expr c d)
bitraverseEitherTable f g e@(EitherTable tag _ _) = do
  mc@(MaybeTable _ c) <- optional (f =<< keepLeftTable e)
  md@(MaybeTable _ d) <- optional (g =<< keepRightTable e)
  where_ $ isJustTable mc ==. isLeftTable e
  where_ $ isJustTable md ==. isRightTable e
  pure $ EitherTable tag c d
