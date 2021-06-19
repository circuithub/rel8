{-# language DataKinds #-}

module Rel8.Query.Exists
  ( exists, inQuery
  , present, with, withBy
  , absent, without, withoutBy
  )
where

-- base
import Prelude hiding ( filter )

-- opaleye
import qualified Opaleye.Operators as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( filter )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Eq ( EqTable, (==:) )
import Rel8.Table.Maybe ( isJustTable )


-- | Checks if a query returns at least one row.
exists :: Query a -> Query (Expr Bool)
exists = fmap isJustTable . optional . present
-- FIXME: change this when b7aacc07c6392654cae439fc3b997620c3aa7a87 makes it
-- into a release of Opaleye


inQuery :: EqTable a => a -> Query a -> Query (Expr Bool)
inQuery a = exists . (>>= filter (a ==:))


-- | Produce the empty query if the given query returns no rows. @present@
-- is equivalent to @WHERE EXISTS@ in SQL.
present :: Query a -> Query ()
present = mapOpaleye Opaleye.restrictExists


-- | Produce the empty query if the given query returns rows. @absent@
-- is equivalent to @WHERE NOT EXISTS@ in SQL.
absent :: Query a -> Query ()
absent = mapOpaleye Opaleye.restrictNotExists


-- | @with@ is similar to 'filter', but allows the predicate to be a full query.
--
-- @with f a = a <$ present (f a)@, but this form matches 'filter'.
with :: (a -> Query b) -> a -> Query a
with f a = a <$ present (f a)


-- | Like @with@, but with a custom membership test.
withBy :: (a -> b -> Expr Bool) -> Query b -> a -> Query a
withBy predicate bs = with $ \a -> bs >>= filter (predicate a)


-- | Filter rows where @a -> Query b@ yields no rows.
without :: (a -> Query b) -> a -> Query a
without f a = a <$ absent (f a)


-- | Like @without@, but with a custom membership test.
withoutBy :: (a -> b -> Expr Bool) -> Query b -> a -> Query a
withoutBy predicate bs = without $ \a -> bs >>= filter (predicate a)
