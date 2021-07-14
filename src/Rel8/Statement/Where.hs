{-# language MonoLocalBinds #-}

module Rel8.Statement.Where
  ( ppWhere
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), text )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( toPrimExpr )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Table.Opaleye ( attributes )


ppWhere :: Selects names exprs
  => TableSchema names -> (exprs -> Expr Bool) -> Doc
ppWhere schema where_ = text "WHERE" <+> ppExpr condition
  where
    ppExpr = Opaleye.ppSqlExpr . Opaleye.sqlExpr . toPrimExpr
    condition = where_ (attributes schema)
