{-# LANGUAGE FlexibleContexts #-}

module Rel8.Query.Loop (
  loop,
) where

-- base
import Prelude

-- opaleye
import Opaleye.With (withRecursiveExplicit)

-- rel8
import Rel8.Expr (Expr)
import Rel8.Query (Query)
import Rel8.Query.Opaleye (fromOpaleye, toOpaleye)
import Rel8.Table (Table)
import Rel8.Table.Opaleye (binaryspec)


{- | 'loop' allows the construction of recursive queries, using Postgres'
[@WITH RECURSIVE@](https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-RECURSIVE)
under the hood. The first argument to 'loop' is what the Postgres
documentation refers to as the \"non-recursive term\" and the second
argument is the \"recursive term\", which is defined in terms of the result
of the \"non-recursive term\".
-}
loop :: Table Expr a => Query a -> (a -> Query a) -> Query a
loop base recurse =
  fromOpaleye $ withRecursiveExplicit binaryspec base' recurse'
  where
    base' = toOpaleye base
    recurse' = toOpaleye . recurse
