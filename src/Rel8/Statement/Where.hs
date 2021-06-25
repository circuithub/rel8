{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}
{-# language StrictData #-}

module Rel8.Statement.Where
  ( Where, ppWhere
  )
where

-- base
import Data.Functor ( (<&>) )
import Prelude

-- pretty
import Text.PrettyPrint ( Doc, ($$), parens, text )

-- rel8
import Rel8.Expr.Bool ( true )
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Statement.Select ( ppSelect )
import Rel8.Table.Opaleye ( view )


-- | The @WHERE@ condition in a @DELETE@ or @UPDATE@ statement. This can be
-- an arbitrary 'Query': if the query returns zero rows, then the condition
-- is false, if the query returns one or more rows, then the condition is
-- true. Most of the time you'll want to use 'Rel8.where_' to supply an
-- explicit boolean value.
type Where expr = expr -> Query ()


ppWhere :: Selects names exprs => TableSchema names -> Where exprs -> Maybe Doc
ppWhere TableSchema {columns} where_ =
  ppSelect (true <$ where_ (view columns)) <&> \query ->
    text "WHERE EXISTS" $$ parens query
