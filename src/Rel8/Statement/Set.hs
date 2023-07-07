{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rel8.Statement.Set (
  ppSet,
)
where

-- base
import Data.Foldable (toList)
import Prelude ()

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint (Doc, equals, text, (<+>))

-- rel8
import Rel8.Schema.Name (Selects, ppColumn)
import Rel8.Schema.Table (TableSchema (..))
import Rel8.Table.Opaleye (attributes, exprsWithNames)


ppSet ::
  Selects names exprs =>
  TableSchema names ->
  (exprs -> exprs) ->
  Doc
ppSet schema@TableSchema{columns} f =
  text "SET" <+> Opaleye.commaV ppAssign (toList assigns)
  where
    assigns = exprsWithNames columns (f (attributes schema))
    ppAssign (column, expr) =
      ppColumn column <+> equals <+> Opaleye.ppSqlExpr (Opaleye.sqlExpr expr)
