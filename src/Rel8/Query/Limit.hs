{-# language LambdaCase #-}

module Rel8.Query.Limit
  ( limit
  , offset
  )
where

-- base
import Data.Int (Int64)
import Prelude

-- opaleye
import qualified Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (toColumn, toPrimExpr)
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )


-- | @limit n@ select at most @n@ rows from a query.  @limit n@ is equivalent
-- to the SQL @LIMIT n@.
limit :: Expr Int64 -> Query a -> Query a
limit = mapOpaleye . Opaleye.limitField . toColumn . stripCast . toPrimExpr


-- | @offset n@ drops the first @n@ rows from a query. @offset n@ is equivalent
-- to the SQL @OFFSET n@.
offset :: Expr Int64 -> Query a -> Query a
offset = mapOpaleye . Opaleye.offsetField . toColumn . stripCast . toPrimExpr


stripCast :: Opaleye.PrimExpr -> Opaleye.PrimExpr
stripCast = \case
  Opaleye.CastExpr "\"int8\"" a -> a
  a -> a
