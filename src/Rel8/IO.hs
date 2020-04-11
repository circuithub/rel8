-- |

module Rel8.IO where

import Database.PostgreSQL.Simple ( Connection, queryWith_ )
import Rel8.Query
import Rel8.Expr

select :: Connection -> Query () (Expr a) -> IO [a]
select c _ = queryWith_ undefined c undefined
