{-# LANGUAGE FlexibleContexts #-}

module Rel8.Query.Rebind (
  rebind,
  hrebind,
)
where

-- base

-- base
import Control.Arrow ((<<<))
import Prelude

-- opaleye
import qualified Opaleye.Internal.Rebind as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Query (Query)
import Rel8.Query.Opaleye (fromOpaleye)
import Rel8.Schema.HTable (HTable)
import Rel8.Table (Table)
import Rel8.Table.Cols (Cols (Cols))
import Rel8.Table.Opaleye (unpackspec)


{- | 'rebind' takes a variable name, some expressions, and binds each of them
to a new variable in the SQL. The @a@ returned consists only of these
variables. It's essentially a @let@ binding for Postgres expressions.
-}
rebind :: Table Expr a => String -> a -> Query a
rebind prefix a = fromOpaleye (Opaleye.rebindExplicitPrefix prefix unpackspec <<< pure a)


hrebind :: HTable t => String -> t Expr -> Query (t Expr)
hrebind prefix = fmap (\(Cols a) -> a) . rebind prefix . Cols
