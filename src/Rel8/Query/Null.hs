module Rel8.Query.Null
  ( catNullable
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( Nullification, isNonNull, nullify, unsafeUnnullify )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )


catNullable :: Nullification nonNullable nullable
  => Expr nullable a -> Query (Expr nonNullable a)
catNullable a = do
  where_ $ isNonNull (nullify a)
  pure $ unsafeUnnullify a
