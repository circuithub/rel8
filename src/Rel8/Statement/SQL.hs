module Rel8.Statement.SQL
  ( showDelete
  , showInsert
  , showUpdate
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Statement.Delete ( Delete, ppDelete )
import Rel8.Statement.Insert ( Insert, ppInsert )
import Rel8.Statement.Update ( Update, ppUpdate )

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
