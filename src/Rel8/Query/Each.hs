{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Query.Each
  ( each
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Table as Opaleye

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Table.Opaleye ( castTable, table, unpackspec )


-- | Select each row from a table definition. This is equivalent to @FROM
-- table@.
each :: Selects names exprs => TableSchema names -> Query exprs
each =
  fmap castTable .
  fromOpaleye .
  Opaleye.selectTableExplicit unpackspec .
  table
