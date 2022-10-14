module Rel8.Query.Indexed
  ( indexed
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Int ( Int64 )
import Prelude ()

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Window ( rowNumber )
import Rel8.Query ( Query )
import Rel8.Query.Window ( window )
import Rel8.Table.Window ( currentRow )


-- | Pair each row of a query with its index within the query.
indexed :: Query a -> Query (Expr Int64, a)
indexed = window (liftA2 (,) rowNumber currentRow)
