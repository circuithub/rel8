{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Internal.Query.Function
  ( queryFunction
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Operators as Opaleye

-- rel8
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Function (Arguments, primFunction)
import Rel8.Internal.Query (Query)
import Rel8.Internal.Query.Opaleye (fromOpaleye)
import Rel8.Internal.Schema.QualifiedName (QualifiedName)
import Rel8.Internal.Table (Table)
import Rel8.Internal.Table.Opaleye (castTable, relExprPP)


-- | Select each row from a function that returns a relation. This is
-- equivalent to @FROM function(input)@.
queryFunction :: (Arguments input, Table Expr output)
  => QualifiedName -> input -> Query output
queryFunction name input = fmap castTable $ fromOpaleye $
  Opaleye.relationValuedExprExplicit relExprPP (const expr)
  where
    expr = primFunction name input
