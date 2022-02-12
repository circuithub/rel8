module Rel8.Query.Indexed
  ( indexed
  )
where

-- base
import Data.Int ( Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )


-- | Pair each row of a query with its index within the query.
indexed :: Query a -> Query (Expr Int64, a)
indexed = mapOpaleye $ \f -> Opaleye.stateQueryArr $ \_ tag ->
  let
    (a, query, tag') = Opaleye.runStateQueryArr f () tag
    tag'' = Opaleye.next tag'
    window = Opaleye.ConstExpr $ Opaleye.OtherLit "ROW_NUMBER() OVER () - 1"
    (index, bindings) = Opaleye.run $ Opaleye.extractAttr "index" tag' window
    query' = query <> Opaleye.aRebind bindings
  in
    ((fromPrimExpr index, a), query', tag'')
