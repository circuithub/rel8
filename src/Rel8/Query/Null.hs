{-# LANGUAGE FlexibleContexts #-}

module Rel8.Query.Null (
  catNull,
  catNullTable,
)
where

-- base
import Prelude

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Null (isNonNull, unsafeUnnullify)
import Rel8.Query (Query)
import Rel8.Query.Filter (where_)
import Rel8.Table (Table)
import Rel8.Table.Null (NullTable, isNonNullTable, unsafeUnnullifyTable)


{- | Filter a 'Query' that might return @null@ to a 'Query' without any
@null@s.

Corresponds to 'Data.Maybe.catMaybes'.
-}
catNull :: Expr (Maybe a) -> Query (Expr a)
catNull a = do
  where_ $ isNonNull a
  pure $ unsafeUnnullify a


{- | Filter a 'Query' that might return @nullTable@ to a 'Query' without any
@nullTable@s.

Corresponds to 'Data.Maybe.catMaybes'.
-}
catNullTable :: Table Expr a => NullTable Expr a -> Query a
catNullTable a = do
  where_ $ isNonNullTable a
  pure $ unsafeUnnullifyTable a
