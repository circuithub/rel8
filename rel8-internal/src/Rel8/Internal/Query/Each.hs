{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Internal.Query.Each
  ( each
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Table as Opaleye

-- rel8
import Rel8.Internal.Query ( Query )
import Rel8.Internal.Query.Opaleye ( fromOpaleye )
import Rel8.Internal.Schema.Name ( Selects )
import Rel8.Internal.Schema.Table ( TableSchema )
import Rel8.Internal.Table.Cols ( fromCols, toCols )
import Rel8.Internal.Table.Opaleye ( table, unpackspec )


-- | Select each row from a table definition. This is equivalent to @FROM
-- table@.
each :: Selects names exprs => TableSchema names -> Query exprs
each =
  fmap fromCols .
  fromOpaleye .
  Opaleye.selectTableExplicit unpackspec .
  table .
  fmap toCols
