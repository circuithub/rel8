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


filter :: (a -> Expr Bool) -> a -> Query a
filter f a = a <$ where_ (f a)


-- | Drop any rows that don't match a predicate.  @where_ expr@ is equivalent
-- to the SQL @WHERE expr@.
--
-- >>> :{
-- select c $ do
--   x <- values [ lit x | x <- [ 1..5 :: Int32 ] ]
--   where_ $ x >. lit 2
--   return x
-- :}
-- [3,4,5]
where_ :: Expr Bool -> Query ()
where_ condition =
  fromOpaleye $ lmap (\_ -> toColumn $ toPrimExpr condition) Opaleye.restrict
