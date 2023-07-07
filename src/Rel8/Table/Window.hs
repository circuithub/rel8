{-# LANGUAGE MonoLocalBinds #-}

module Rel8.Table.Window (
  currentRow,
)
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Window as Opaleye

-- rel8
import Rel8.Window (Window (Window))


-- | Return every column of the current row of a window query.
currentRow :: Window a a
currentRow = Window $ Opaleye.over (Opaleye.noWindowFunction id) mempty mempty
