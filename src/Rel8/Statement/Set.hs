{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}

module Rel8.Statement.Set
  ( Set
  , ppSet
  )
where

-- base
import Data.Foldable ( toList )
import Prelude ()

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), equals, text )

-- rel8
import Rel8.Schema.Name ( Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Table.Opaleye ( attributes, exprsWithNames )


-- | The @SET@ part of an @UPDATE@ (or @ON CONFLICT DO UPDATE@) statement.
--
-- The @expr -> expr@ function takes the current value of the existing row and
-- returns the updated values for the row.
--
-- The additional parameter @from@ is either the result of the query executed
-- in the @FROM@ of an @UPDATE@ stateent, or the @excluded@ row that couldn't
-- be inserted in an @ON CONFLICT DO UPDATE@ statement.
type Set from expr = from -> expr -> expr


ppSet :: Selects names exprs
  => TableSchema names -> from -> Set from exprs -> Doc
ppSet schema@TableSchema {columns} from f =
  text "SET" <+> Opaleye.commaV ppAssign (toList assigns)
  where
    assigns = exprsWithNames columns (f from (attributes schema))
    ppAssign (column, expr) =
      ppColumn column <+> equals <+> Opaleye.ppSqlExpr (Opaleye.sqlExpr expr)
