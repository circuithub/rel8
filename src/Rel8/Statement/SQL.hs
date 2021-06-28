{-# language MonoLocalBinds #-}

module Rel8.Statement.SQL
  ( showDelete
  , showInsert
  , showUpdate
  )
where

-- base
import Prelude

-- rel8
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Delete ( Delete, ppDelete )
import Rel8.Statement.Insert ( Insert, ppInsert )
import Rel8.Statement.Update ( Update, ppUpdate )


-- | Convert a 'Delete' to a 'String' containing a @DELETE@ statement.
showDelete :: Selects names exprs => TableSchema names -> Delete exprs a -> String
showDelete schema = foldMap show . ppDelete schema


-- | Convert an 'Insert' to a 'String' containing an @INSERT@ statement.
showInsert :: Selects names exprs => TableSchema names -> Insert exprs a -> String
showInsert schema = foldMap show . ppInsert schema


-- | Convert an 'Update' to a 'String' containing an @UPDATE@ statement.
showUpdate :: Selects names exprs => TableSchema names -> Update exprs a -> String
showUpdate schema = foldMap show . ppUpdate schema
