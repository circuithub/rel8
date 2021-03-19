{-# language FlexibleContexts #-}

module Rel8.Query.List
  ( many, some
  )
where

-- base
import Prelude

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.Aggregate ( aggregate )
import Rel8.Query.Maybe ( optional )
import Rel8.Schema.Context ( DB )
import Rel8.Table ( Table )
import Rel8.Table.Aggregate ( listAgg, nonEmptyAgg )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( maybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )


some :: Table DB a => Query a -> Query (NonEmptyTable a)
some = aggregate . fmap nonEmptyAgg


many :: Table DB a => Query a -> Query (ListTable a)
many = fmap (maybeTable mempty id) . optional . aggregate . fmap listAgg
