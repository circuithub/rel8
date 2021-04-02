module Rel8.Query.Filter
  ( filter
  , where_
  )
where

-- base
import Prelude hiding ( filter )

-- opaleye
import qualified Opaleye.Operators as Opaleye

-- profunctors
import Data.Profunctor ( lmap )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( toColumn, toPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye )


-- | @filter f x@ will be a zero-row query when @f x@ is @False@, and will
-- return @x@ unchanged when @f x@ is @True@. This is similar to
-- 'Control.Monad.guard', but as the predicate is separate from the argument,
-- it is easy to use in a pipeline of 'Query' transformations.
filter :: (a -> Expr Bool) -> a -> Query a
filter f a = a <$ where_ (f a)


-- | Drop any rows that don't match a predicate.  @where_ expr@ is equivalent
-- to the SQL @WHERE expr@.
where_ :: Expr Bool -> Query ()
where_ condition =
  fromOpaleye $ lmap (\_ -> toColumn $ toPrimExpr condition) Opaleye.restrict
