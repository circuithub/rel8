{-# language FlexibleContexts #-}

module Rel8.Query.Evaluate
  ( eval
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


-- | 'eval' takes expressions that could potentially have side effects and
-- \"runs\" them in the 'Query' monad. The returned expressions have no
-- side effetcs and can safely be reused.
eval :: Table Expr a => a -> Query a
eval a = Query $ Opaleye.QueryArr $ \(_, query, tag) ->
  let
    tag' = Opaleye.next tag
    (a', bindings) = Opaleye.run $
      Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "eval" tag') a
  in
    (a', Opaleye.Rebind True bindings query, tag')
