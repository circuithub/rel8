{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Query.Distinct
  ( distinct
  , distinctOn
  , distinctOnBy
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Internal.Order as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye

-- rel8
import Rel8.Order ( Order( Order ) )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.Opaleye ( distinctspec, unpackspec )


-- | Select all distinct rows from a query, removing duplicates.  @distinct q@
-- is equivalent to the SQL statement @SELECT DISTINCT q@.
--
-- >>> select c $ distinct $ values [ lit True, lit True, lit False ]
-- [False,True]
distinct :: EqTable a => Query a -> Query a
distinct = mapOpaleye (Opaleye.distinctExplicit distinctspec)


-- | Select all distinct rows from a query, where rows are equivalent according
-- to a projection. If multiple rows have the same projection, it is
-- unspecified which row will be returned. If this matters, use 'distinctOnBy'.
distinctOn :: EqTable b => (a -> b) -> Query a -> Query a
distinctOn proj =
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOn unpackspec proj . Opaleye.runSimpleQueryArr q))


-- | Select all distinct rows from a query, where rows are equivalent according
-- to a projection. If there are multiple rows with the same projection, the
-- first row according to the specified 'Order' will be returned.
distinctOnBy :: EqTable b => (a -> b) -> Order a -> Query a -> Query a
distinctOnBy proj (Order order) =
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOnBy unpackspec proj order . Opaleye.runSimpleQueryArr q))
