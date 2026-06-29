module Rel8.Internal.Query.Indexed
  ( indexed
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Int ( Int64 )
import Prelude

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Expr.Window ( rowNumber )
import Rel8.Internal.Query ( Query )
import Rel8.Internal.Query.Window ( window )
import Rel8.Internal.Table.Window ( currentRow )


-- | Pair each row of a query with its index within the query.
indexed :: Query a -> Query (Expr Int64, a)
indexed = window (liftA2 (,) (subtract 1 <$> rowNumber) currentRow)
