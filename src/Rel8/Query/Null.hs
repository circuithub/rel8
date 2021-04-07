module Rel8.Query.Null
  ( catNull
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( isNonNull, unsafeUnnullify )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )


-- | Filter a 'Query' that might return @null@ to a 'Query' without any
-- @null@s.
--
-- Corresponds to 'Data.Maybe.catMaybes'.
catNull :: Expr (Maybe a) -> Query (Expr a)
catNull a = do
  where_ $ isNonNull a
  pure $ unsafeUnnullify a
