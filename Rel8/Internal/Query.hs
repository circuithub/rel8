module Rel8.Internal.Query where

import Rel8.Internal.Table
import qualified Opaleye as O

aggregate
  :: AggregateTable table result
  => O.Query table -> O.Query result
aggregate = O.aggregate traverseAggregates
