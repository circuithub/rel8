{-# language FlexibleContexts #-}

module Rel8.Statement.SQL
  ( showDelete
  , showInsert
  , showUpdate
  , showStatement
  , showPreparedStatement
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Statement (Statement, ppDecodeStatement)
import Rel8.Statement.Delete ( Delete, ppDelete )
import Rel8.Statement.Insert ( Insert, ppInsert )
import Rel8.Statement.Prepared (input)
import Rel8.Statement.Rows (Rows (Void))
import Rel8.Statement.Select (ppSelect)
import Rel8.Statement.Update ( Update, ppUpdate )
import Rel8.Table (Table)

-- transformers
import Control.Monad.Trans.State.Strict (evalState)


-- | Convert a 'Delete' to a 'String' containing a @DELETE@ statement.
showDelete :: Delete a -> String
showDelete = show . (`evalState` Opaleye.start) . ppDelete


-- | Convert an 'Insert' to a 'String' containing an @INSERT@ statement.
showInsert :: Insert a -> String
showInsert = show . (`evalState` Opaleye.start) . ppInsert


-- | Convert an 'Update' to a 'String' containing an @UPDATE@ statement.
showUpdate :: Update a -> String
showUpdate = show . (`evalState` Opaleye.start) . ppUpdate


-- | Convert a 'Statement' to a 'String' containing an SQL statement.
showStatement :: Statement a -> String
showStatement = show . fst . ppDecodeStatement ppSelect Void


-- | Convert a parameterized 'Statement' to a 'String' containing an SQL
-- statement.
showPreparedStatement :: Table Expr i => (i -> Statement a) -> String
showPreparedStatement = showStatement . ($ input)
