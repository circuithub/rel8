{-# language FlexibleContexts #-}

module Rel8.Query.Table
  ( TableSchema(..)
  , each
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Table as Opaleye

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye )
import Rel8.Schema.Context ( Name, DB )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Table.Map ( MapTable )
import Rel8.Table.Opaleye ( TableSchema(..), table, unpackspec )


each :: MapTable Name DB names exprs => TableSchema names -> Query exprs
each =
  fmap fromColumns .
  fromOpaleye .
  Opaleye.selectTableExplicit unpackspec .
  table .
  fmap toColumns
