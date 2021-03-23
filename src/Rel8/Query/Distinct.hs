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


distinct :: EqTable a => Query a -> Query a
distinct = mapOpaleye (Opaleye.distinctExplicit distinctspec)


distinctOn :: EqTable b => (a -> b) -> Query a -> Query a
distinctOn proj =
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOn unpackspec proj . Opaleye.runSimpleQueryArr q))


distinctOnBy :: EqTable b => (a -> b) -> Order a -> Query a -> Query a
distinctOnBy proj (Order order) =
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOnBy unpackspec proj order . Opaleye.runSimpleQueryArr q))
