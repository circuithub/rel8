{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}

module Rel8.Query.List
  ( many, some
  , manyExpr, someExpr
  , catListTable, catNonEmptyTable
  , catList, catNonEmpty
  )
where

-- base
import Control.Monad ((>=>))
import Data.Functor.Identity ( runIdentity )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( listAggExpr, nonEmptyAggExpr )
import Rel8.Expr.Opaleye ( mapPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Aggregate (aggregate, aggregate1)
import Rel8.Query.Rebind (hrebind, rebind)
import Rel8.Schema.HTable (HTable, hfield, hspecs, htabulate)
import Rel8.Schema.HTable.Vectorize ( hunvectorize )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Spec ( Spec( Spec, info ) )
import Rel8.Table (Table, fromColumns, toColumns)
import Rel8.Table.Aggregate ( listAgg, nonEmptyAgg )
import Rel8.Table.List ( ListTable( ListTable ) )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )
import Rel8.Type ( DBType )
import Rel8.Type.Array ( extractArrayElement )


-- | Aggregate a 'Query' into a 'ListTable'. If the supplied query returns 0
-- rows, this function will produce a 'Query' that returns one row containing
-- the empty @ListTable@. If the supplied @Query@ does return rows, @many@ will
-- return exactly one row, with a @ListTable@ collecting all returned rows.
--
-- @many@ is analogous to 'Control.Applicative.many' from
-- @Control.Applicative@.
many :: Table Expr a => Query a -> Query (ListTable Expr a)
many = aggregate listAgg


-- | Aggregate a 'Query' into a 'NonEmptyTable'. If the supplied query returns
-- 0 rows, this function will produce a 'Query' that is empty - that is, will
-- produce zero @NonEmptyTable@s. If the supplied @Query@ does return rows,
-- @some@ will return exactly one row, with a @NonEmptyTable@ collecting all
-- returned rows.
--
-- @some@ is analogous to 'Control.Applicative.some' from
-- @Control.Applicative@.
some :: Table Expr a => Query a -> Query (NonEmptyTable Expr a)
some = aggregate1 nonEmptyAgg


-- | A version of 'many' specialised to single expressions.
manyExpr :: Sql DBType a => Query (Expr a) -> Query (Expr [a])
manyExpr = aggregate listAggExpr


-- | A version of 'some' specialised to single expressions.
someExpr :: Sql DBType a => Query (Expr a) -> Query (Expr (NonEmpty a))
someExpr = aggregate1 nonEmptyAggExpr


-- | Expand a 'ListTable' into a 'Query', where each row in the query is an
-- element of the given @ListTable@.
--
-- @catListTable@ is an inverse to 'many'.
catListTable :: Table Expr a => ListTable Expr a -> Query a
catListTable (ListTable as) =
  fmap fromColumns $ (hrebind "unnest" >=> hextract) $ runIdentity $
    hunvectorize (\_ -> pure . unnest) as


-- | Expand a 'NonEmptyTable' into a 'Query', where each row in the query is an
-- element of the given @NonEmptyTable@.
--
-- @catNonEmptyTable@ is an inverse to 'some'.
catNonEmptyTable :: Table Expr a => NonEmptyTable Expr a -> Query a
catNonEmptyTable (NonEmptyTable as) =
  fmap fromColumns $ (hrebind "unnest" >=> hextract) $ runIdentity $
    hunvectorize (\_ -> pure . unnest) as


-- | Expand an expression that contains a list into a 'Query', where each row
-- in the query is an element of the given list.
--
-- @catList@ is an inverse to 'manyExpr'.
catList :: Sql DBType a => Expr [a] -> Query (Expr a)
catList = rebind "unnest" . unnest >=> extract


-- | Expand an expression that contains a non-empty list into a 'Query', where
-- each row in the query is an element of the given list.
--
-- @catNonEmpty@ is an inverse to 'someExpr'.
catNonEmpty :: Sql DBType a => Expr (NonEmpty a) -> Query (Expr a)
catNonEmpty = rebind "unnest" . unnest >=> extract


unnest :: Expr (list a) -> Expr a
unnest = mapPrimExpr $ Opaleye.UnExpr (Opaleye.UnOpOther "UNNEST")


extract :: Table Expr a => a -> Query a
extract = fmap fromColumns . hextract . toColumns


hextract :: HTable t => t Expr -> Query (t Expr)
hextract = hrebind "extract" . go
  where
    go as = htabulate $ \field ->
      case hfield as field of
        a -> case hfield hspecs field of
          Spec {info} -> mapPrimExpr (extractArrayElement info) a
