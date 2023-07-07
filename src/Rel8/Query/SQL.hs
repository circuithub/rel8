{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Rel8.Query.SQL (
  showQuery,
)
where

-- base
import Prelude

-- rel8
import Rel8.Expr (Expr)
import Rel8.Query (Query)
import Rel8.Statement.Select (ppSelect)
import Rel8.Table (Table)


-- | Convert a 'Query' to a 'String' containing a @SELECT@ statement.
showQuery :: Table Expr a => Query a -> String
showQuery = show . ppSelect
