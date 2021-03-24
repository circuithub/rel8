{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Insert
  ( toInsert
  , toInsertDefaults
  )
where

-- base
import Prelude

-- rel8
import Rel8.Expr ( Col(..) )
import Rel8.Kind.Necessity ( SNecessity( SOptional, SRequired ) )
import Rel8.Schema.HTable ( hfield, htabulate, hspecs )
import Rel8.Schema.Insert ( Inserts, Col(..) )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Table ( fromColumns, toColumns )


-- | @toInsert@ converts a 'Table' of 'Expr's into a 'Table' that can be used
-- with 'insert'. This will override any columns that have default values to
-- use exactly what is given. If you want to use default values, you can either
-- override the result of @toInsert@, or use 'toInsertDefaults'.
toInsert :: Inserts exprs inserts => exprs -> inserts
toInsert (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {necessity} -> case hfield exprs field of
      DB expr -> case necessity of
        SRequired -> RequiredInsert expr
        SOptional -> OptionalInsert (Just expr)


-- | @toInsertDefaults@ converts a 'Table' of 'Expr's into a 'Table' that can
-- be used with 'insert'. Any columns that have a default value will override
-- whatever is in the input expression. 
--
-- One example where this is useful is for any table that has a special @id@
-- column, which has a default value to draw a new value from a sequence. If we
-- use 'toInsertDefaults', we can provide a dummy value that will be replaced
-- with a call to @DEFAULT@.
toInsertDefaults :: Inserts exprs inserts => exprs -> inserts
toInsertDefaults (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {necessity} -> case hfield exprs field of
      DB expr -> case necessity of
        SRequired -> RequiredInsert expr
        SOptional -> OptionalInsert Nothing
