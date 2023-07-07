{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rel8.Query.Maybe (
  optional,
  catMaybeTable,
  traverseMaybeTable,
)
where

-- base
import Prelude

-- comonad
import Control.Comonad (extract)

-- opaleye
import qualified Opaleye.Internal.MaybeFields as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Eq ((==.))
import Rel8.Query (Query)
import Rel8.Query.Filter (where_)
import Rel8.Query.Opaleye (mapOpaleye)
import Rel8.Table.Maybe (MaybeTable (..), isJustTable, makeMaybeTable)


{- | Convert a query that might return zero rows to a query that always returns
at least one row.

To speak in more concrete terms, 'optional' is most useful to write @LEFT
JOIN@s.
-}
optional :: Query a -> Query (MaybeTable Expr a)
optional = mapOpaleye $ Opaleye.optionalInternal makeMaybeTable


{- | Filter out 'MaybeTable's, returning only the tables that are not-null.

This operation can be used to "undo" the effect of 'optional', which
operationally is like turning a @LEFT JOIN@ back into a full @JOIN@.  You
can think of this as analogous to 'Data.Maybe.catMaybes'.
-}
catMaybeTable :: MaybeTable Expr a -> Query a
catMaybeTable ma@(MaybeTable _ a) = do
  where_ $ isJustTable ma
  pure (extract a)


{- | Extend an optional query with another query.  This is useful if you want
to step through multiple @LEFT JOINs@.

Note that @traverseMaybeTable@ takes a @a -> Query b@ function, which means
you also have the ability to "expand" one row into multiple rows.  If the
@a -> Query b@ function returns no rows, then the resulting query will also
have no rows. However, regardless of the given @a -> Query b@ function, if
the input is @nothingTable@, you will always get exactly one @nothingTable@
back.
-}
traverseMaybeTable :: (a -> Query b) -> MaybeTable Expr a -> Query (MaybeTable Expr b)
traverseMaybeTable query ma@(MaybeTable input _) = do
  optional (query =<< catMaybeTable ma) >>= \case
    MaybeTable output b -> do
      where_ $ output ==. input
      pure $ MaybeTable input b
