module Rel8.Expr.Sequence
  ( nextval
  )
where

-- base
import Data.Int ( Int64 )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Function ( function )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Expr.Text ( quoteIdent )

-- text
import Data.Text ( pack )


-- | See https://www.postgresql.org/docs/current/functions-sequence.html
nextval :: String -> Expr Int64
nextval = function "nextval" . quoteIdent . litExpr . pack
