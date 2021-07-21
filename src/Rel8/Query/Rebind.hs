{-# language FlexibleContexts #-}

module Rel8.Query.Rebind
  ( rebind
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query( Query ) )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( unpackspec )


-- | 'rebind' takes a variable name, some expressions, and binds each of them
-- to a new variable in the SQL. The @a@ returned consists only of these
-- variables. It's essentially a @let@ binding for Postgres expressions.
rebind :: Table Expr a => String -> a -> Query a
rebind prefix a = Query $ \_ -> Opaleye.QueryArr $ \(_, tag) ->
  let
    tag' = Opaleye.next tag
    (a', bindings) = Opaleye.run $
      Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr prefix tag) a
  in
    ((mempty, a'), \_ -> Opaleye.Rebind True bindings, tag')
