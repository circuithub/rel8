{-# language FlexibleContexts #-}

module Rel8.Internal.Query.Null
  ( catNull
  , catNullTable
  )
where

-- base
import Prelude

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Expr.Null ( isNonNull, unsafeUnnullify )
import Rel8.Internal.Table ( Table )
import Rel8.Internal.Table.Null ( NullTable, isNonNullTable, unsafeUnnullifyTable )
import Rel8.Internal.Query ( Query )
import Rel8.Internal.Query.Filter ( where_ )


-- | Filter a 'Query' that might return @null@ to a 'Query' without any
-- @null@s.
--
-- Corresponds to 'Data.Maybe.catMaybes'.
catNull :: Expr (Maybe a) -> Query (Expr a)
catNull a = do
  where_ $ isNonNull a
  pure $ unsafeUnnullify a


-- | Filter a 'Query' that might return @nullTable@ to a 'Query' without any
-- @nullTable@s.
--
-- Corresponds to 'Data.Maybe.catMaybes'.
catNullTable :: Table Expr a => NullTable Expr a -> Query a
catNullTable a = do
  where_ $ isNonNullTable a
  pure $ unsafeUnnullifyTable a
