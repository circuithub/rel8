{-# language FlexibleContexts #-}

module Rel8.Internal.Query.Rebind
  ( rebind
  , hrebind
  )
where

-- base
import Prelude
import Control.Arrow ((<<<))

-- opaleye
import qualified Opaleye.Internal.Rebind as Opaleye

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Query ( Query )
import Rel8.Internal.Query.Limit (offset)
import Rel8.Internal.Schema.HTable (HTable)
import Rel8.Internal.Table ( Table )
import Rel8.Internal.Table.Cols (Cols (Cols))
import Rel8.Internal.Table.Opaleye ( unpackspec )
import Rel8.Internal.Query.Opaleye (fromOpaleye)


-- | 'rebind' takes a variable name, some expressions, and binds each of them
-- to a new variable in the SQL. The @a@ returned consists only of these
-- variables. It's essentially a @let@ binding for Postgres expressions.
rebind :: Table Expr a => String -> a -> Query a
rebind prefix a = offset 0 $
  fromOpaleye (Opaleye.rebindExplicitPrefix prefix unpackspec <<< pure a)


hrebind :: HTable t => String -> t Expr -> Query (t Expr)
hrebind prefix = fmap (\(Cols a) -> a) . rebind prefix . Cols
