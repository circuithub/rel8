module Rel8.Array
  (
    -- ** @ListTable@
    ListTable
  , head, headExpr
  , index, indexExpr
  , last, lastExpr
  , length, lengthExpr

    -- ** @NonEmptyTable@
  , NonEmptyTable
  , head1, head1Expr
  , index1, index1Expr
  , last1, last1Expr
  , length1, length1Expr
  )
where

-- base
import Prelude hiding (head, last, length)

-- rel8
import Rel8.Expr.List
import Rel8.Expr.NonEmpty
import Rel8.Table.List
import Rel8.Table.NonEmpty
