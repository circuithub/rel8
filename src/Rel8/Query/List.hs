{-# language FlexibleContexts #-}

module Rel8.Query.List
  ( many, some
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Query.Aggregate ( aggregate )
import Rel8.Query.Maybe ( optional )
import Rel8.Table ( Table )
import Rel8.Table.Aggregate ( listAgg, nonEmptyAgg )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( maybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )


-- | Aggregate a 'Query' into a 'NonEmptyTable'. If the supplied query returns
-- 0 rows, this function will produce a 'Query' that is empty - that is, will
-- produce zero @NonEmptyTable@s. If the supplied @Query@ does return rows,
-- @some@ will return exactly one row, with a @NonEmptyTable@ collecting all
-- returned rows.
--
-- @some@ is analogous to 'Control.Applicative.some' from
-- @Control.Applicative@.
some :: Table Expr a => Query a -> Query (NonEmptyTable a)
some = aggregate (nonEmptyAgg id)


-- | Aggregate a 'Query' into a 'ListTable'. If the supplied query returns 0
-- rows, this function will produce a 'Query' that returns one row containing
-- the empty @ListTable@. If the supplied @Query@ does return rows, @many@ will
-- return exactly one row, with a @ListTable@ collecting all returned rows.
-- 
-- @many@ is analogous to 'Control.Applicative.many' from
-- @Control.Applicative@.
many :: Table Expr a => Query a -> Query (ListTable a)
many = fmap (maybeTable mempty id) . optional . aggregate (listAgg id)
