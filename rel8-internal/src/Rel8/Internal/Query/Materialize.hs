{-# language FlexibleContexts #-}

module Rel8.Internal.Query.Materialize
  ( materialize
  )
where

-- base
import Prelude

-- opaleye
import Opaleye.With ( withMaterializedExplicit )

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Query ( Query )
import Rel8.Internal.Query.Opaleye ( fromOpaleye, toOpaleye )
import Rel8.Internal.Table ( Table )
import Rel8.Internal.Table.Opaleye ( unpackspec )


-- | 'materialize' takes a 'Query' and fully evaluates it and caches the
-- results thereof, and passes to a continuation a new 'Query' that simply
-- looks up these cached results. It's usually best not to use this and to let
-- the Postgres optimizer decide for itself what's best, but if you know what
-- you're doing this can sometimes help to nudge it in a particular direction.
--
-- 'materialize' is currently implemented in terms of Postgres'
-- [@WITH](https://www.postgresql.org/docs/current/queries-with.html) syntax,
-- specifically the @WITH _ AS MATERIALIZED (_)@ form introduced in PostgreSQL
-- 12. This means that 'materialize' can only be used with PostgreSQL 12 or
-- newer.
materialize :: Table Expr a => Query a -> (Query a -> Query b) -> Query b
materialize query f =
  fromOpaleye $
    withMaterializedExplicit unpackspec
      (toOpaleye query')
      (toOpaleye . f . fromOpaleye)
  where
    query' = query
