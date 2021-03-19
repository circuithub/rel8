module Rel8.Query.Either
  ( keepLeftTable
  , keepRightTable
  )
where

-- base
import Prelude

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )
import Rel8.Table.Either
  ( EitherTable( EitherTable )
  , isLeftTable, isRightTable
  )


keepLeftTable :: EitherTable a b -> Query a
keepLeftTable e@(EitherTable _ a _) = do
  where_ $ isLeftTable e
  pure a


keepRightTable :: EitherTable a b -> Query b
keepRightTable e@(EitherTable _ _ b) = do
  where_ $ isRightTable e
  pure b
