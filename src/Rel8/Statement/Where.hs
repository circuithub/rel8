{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Statement.Where
  ( Where
  , ppWhere
  , Restrict
  , restrict
  )
where

-- base
import Data.Functor ( (<&>) )
import Data.Kind ( Constraint, Type )
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
type Where :: Type -> Type
type Where expr = expr -> Query ()


ppWhere :: Selects names exprs => TableSchema names -> Where exprs -> Maybe Doc
ppWhere TableSchema {columns} where_ =
  ppSelect (true <$ where_ (view columns)) <&> \query ->
    text "WHERE EXISTS" $$ parens query


-- | 'Rel8.Update' and 'Rel8.Delete' both support restricting the statement
-- to only operate on rows matching a given 'Where' predicate.
type Restrict :: (Type -> Type -> Type) -> Constraint
class Restrict m where
  -- | Only operate on rows matching the given 'Where' predicate.
  restrict :: Where exprs -> m exprs ()
