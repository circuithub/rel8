{-# language TypeFamilies #-}

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
import Rel8.Table ( Table, Context, toColumns )
import Rel8.Table.Aggregate ( listAgg, nonEmptyAgg )
import Rel8.Table.List ( ListTable( ListTable ) )
import Rel8.Table.Maybe ( maybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )


some :: (Table a, Context a ~ DB) => Query a -> Query (NonEmptyTable a)
some =
  fmap NonEmptyTable .
  aggregate .
  fmap ((\(NonEmptyTable a) -> a) . nonEmptyAgg . toColumns)


many :: (Table a, Context a ~ DB) => Query a -> Query (ListTable a)
many =
  fmap (maybeTable mempty ListTable) .
  optional .
  aggregate .
  fmap ((\(ListTable a) -> a) . listAgg . toColumns)
