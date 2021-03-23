module Rel8.Query.Null
  ( catNullable
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
-- 
-- >>> select c $ pure (nullExpr :: Expr (Maybe Bool))
-- [Nothing]
-- 
-- >>> select c $ catNullable (nullExpr :: Expr (Maybe Bool))
-- []
-- 
-- >>> select c $ catNullable (lit (Just True))
-- [True]
-- 
-- Notice how in the last example a @Bool@ is returned (rather than @Maybe
-- Bool@):
-- 
-- >>> :t catNullable (lit (Just True))
-- catMaybe (lit (Just True)) :: Query (Expr Bool)
catNullable :: Expr (Maybe a) -> Query (Expr a)
catNullable a = do
  where_ $ isNonNull a
  pure $ unsafeUnnullify a
