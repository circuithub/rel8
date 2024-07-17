{-# language FlexibleContexts #-}

module Rel8.Query.Loop
  ( loop
  , loopDistinct
  ) where

-- base
import Prelude

-- opaleye
import Opaleye.With (withRecursiveExplicit, withRecursiveDistinctExplicit)

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye, toOpaleye )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( binaryspec )


-- | 'loop' allows the construction of recursive queries, using Postgres'
-- [@WITH RECURSIVE@](https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-RECURSIVE)
-- under the hood. The first argument to 'loop' is what the Postgres
-- documentation refers to as the \"non-recursive term\" and the second
-- argument is the \"recursive term\", which is defined in terms of the result
-- of the \"non-recursive term\". 'loop' uses @UNION ALL@ to combine the
-- recursive and non-recursive terms.
--
-- Denotionally, @'loop' s f@ is the smallest set of rows @r@ such
-- that
--
-- @
-- r == s \`'Rel8.unionAll'\` (r >>= f)
-- @
--
-- Operationally, @'loop' s f@ takes each row in an initial set @s@ and
-- supplies it to @f@, resulting in a new generation of rows which are added
-- to the result set. Each row from this new generation is then fed back to
-- @f@, and this process is repeated until a generation comes along for which
-- @f@ returns an empty set for each row therein.
loop :: Table Expr a => Query a -> (a -> Query a) -> Query a
loop base recurse =
  fromOpaleye $ withRecursiveExplicit binaryspec base' recurse'
  where
    base' = toOpaleye base
    recurse' = toOpaleye . recurse


-- | 'loopDistinct' is like 'loop' but uses @UNION@ instead of @UNION ALL@ to
-- combine the recursive and non-recursive terms.
--
-- Denotationally, @'loopDistinct' s f@ is the smallest set of rows
-- @r@ such that
--
-- @
-- r == s \`'Rel8.union'\` (r >>= f)
-- @
--
-- Operationally, @'loopDistinct' s f@ takes each /distinct/ row in an
-- initial set @s@ and supplies it to @f@, resulting in a new generation of
-- rows. Any rows returned by @f@ that already exist in the result set are not
-- considered part of this new generation by 'loopDistinct' (in contrast to
-- 'loop'). This new generation is then added to the result set, and each row
-- therein is then fed back to @f@, and this process is repeated until a
-- generation comes along for which @f@ returns no rows that don't already
-- exist in the result set.
loopDistinct :: Table Expr a => Query a -> (a -> Query a) -> Query a
loopDistinct base recurse =
  fromOpaleye $ withRecursiveDistinctExplicit binaryspec base' recurse'
  where
    base' = toOpaleye base
    recurse' = toOpaleye . recurse
