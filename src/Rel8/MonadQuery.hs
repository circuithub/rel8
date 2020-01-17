{-# language FlexibleContexts #-}

module Rel8.MonadQuery where

import Numeric.Natural
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.MaybeTable
import Rel8.Nest
import Rel8.Rewrite
import Rel8.TableSchema


-- | The class of monads that can form SQL queries, along with the corresponding
-- expression type.
class Monad m => MonadQuery m


-- | Exists checks if a query returns at least one row.
--
-- @exists q@ is the same as the SQL expression @EXISTS ( q )@
exists :: m a -> m ( Expr m Bool )
exists = undefined


-- | Select each row from a table definition.
--
-- This is equivalent to @FROM table@.
each :: ( MonadQuery m, Rewrite ColumnSchema ( Expr m ) schema row ) => TableSchema schema -> m row
each =
  undefined


-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, 'nullTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
leftJoin
  :: ( MonadQuery m, Rewrite ( Expr ( Nest m ) ) ( Expr m ) outer' outer )
  => Nest m outer'
  -> ( outer -> Expr m bool )
  -> m ( MaybeTable outer )
leftJoin _ _ =
  undefined


-- | Combine the results of two queries of the same type.
--
-- @union a b@ is the same as the SQL statement @x UNION b@.
union :: m a -> m a -> m a
union = undefined


-- | Select all distinct rows from a query, removing duplicates.
--
-- @distinct q@ is equivalent to the SQL statement @SELECT DISTINCT q@
distinct :: m a -> m a
distinct = undefined


-- | @limit n@ select at most @n@ rows from a query.
--
-- @limit n@ is equivalent to the SQL @LIMIT n@.
limit :: Natural -> m a -> m a
limit = undefined


-- | @offset n@ drops the first @n@ rows from a query.
--
-- @offset n@ is equivalent to the SQL @OFFSET n@.
offset :: Natural -> m a -> m a
offset = undefined


-- | Drop any rows that don't match a predicate.
--
-- @where_ expr@ is equivalent to the SQL @WHERE expr@.
where_ :: MonadQuery m => Expr m Bool -> m ()
where_ =
  undefined
