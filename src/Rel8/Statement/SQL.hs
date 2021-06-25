module Rel8.Statement.SQL
  ( showDelete
  , showInsert
  , showUpdate
  )
where

-- base
import Prelude

-- rel8
import Rel8.Statement.Delete ( Delete, ppDelete )
import Rel8.Statement.Insert ( Insert, ppInsert )
import Rel8.Statement.Update ( Update, ppUpdate )


-- | Convert a 'Delete' to a 'String' containing a @DELETE@ statement.
showDelete :: Delete a -> String
showDelete = foldMap show . ppDelete


-- | Convert an 'Insert' to a 'String' containing an @INSERT@ statement.
showInsert :: Insert a -> String
showInsert = foldMap show . ppInsert


-- | Convert an 'Update' to a 'String' containing an @UPDATE@ statement.
showUpdate :: Update a -> String
showUpdate = foldMap show . ppUpdate
