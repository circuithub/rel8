{-# language FlexibleContexts #-}
{-# language GADTs #-}

module Rel8.Query.Evaluate
  ( eval
  )
where

-- base
import Data.Monoid ( Endo( Endo ) )
import Prelude

-- opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query( Query ) )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( unpackspec )

-- transformers
import Control.Monad.Trans.State.Strict ( get, modify )


-- | 'eval' takes expressions that could potentially have side effects and
-- \"runs\" them in the 'Query' monad. The returned expressions have no
-- side effetcs and can safely be reused.
eval :: Table Expr a => a -> Query a
eval a = Query
  ( do
      tag <- get
      let
        (a', bindings) = Opaleye.run $
          Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "eval" tag) a
      modify Opaleye.next
      pure (Endo (Opaleye.Rebind True bindings), pure a')
  )
