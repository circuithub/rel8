{-# language FlexibleContexts #-}

module Rel8.Query.Materialize
  ( materialize
  ) where

-- base
import Prelude

-- opaleye
import Opaleye.With ( withExplicit )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye, toOpaleye )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( unpackspec )


-- | 'materialize' takes a 'Query' and fully evaluates it and caches the
-- results thereof, and returns a new 'Query' that simply looks up these
-- cached results. It's usually best not to use this and to let the Postgres
-- optimizer decide for itself what's best, but if you know what you're doing
-- this can sometimes help to nudge it in a particular direction.
--
-- 'materialize' is currently implemented in terms of Postgres'
-- [@WITH](https://www.postgresql.org/docs/current/queries-with.html) syntax.
-- Note that on newer versions of PostgreSQL starting with version 12, @WITH@
-- doesn't always automatically materialize if the results of the query aren't
-- used more than once. We reserve the right to change the implementation of
-- 'materialize' to use the newer @WITH foo AS MATERIALIZED bar@ syntax
-- introduced in PostgreSQL 12 in the future. Currently Rel8 does not use
-- @AS MATERIALIZED@ to support earlier PostgreSQL versions.
materialize :: Table Expr a => Query a -> Query (Query a)
materialize query = fromOpaleye $
  withExplicit unpackspec (toOpaleye query) (pure . fromOpaleye)
