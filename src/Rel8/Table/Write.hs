{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Write
  ( defaultValue
  , override
  , write
  , reset
  )
where

-- base
import Prelude

-- rel8
import Rel8.Column ( Default )
import Rel8.Expr ( Col( E ), Expr )
import Rel8.Kind.Defaulting ( SDefaulting( SHasDefault, SNoDefault ) )
import Rel8.Schema.HTable ( hfield, hspecs, htabulate )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Write ( Col( W ), Write, Writes, Writable(..) )
import Rel8.Table ( Table, fromColumns, toColumns )


defaultValue :: Default Write a
defaultValue = Default


override :: Expr a -> Default Write a
override = Value


-- | @write@ converts a 'Table' of 'Expr's into a 'Table' that can be used
-- with 'Rel8.insert'. This will override any columns that have default values
-- to use exactly what is given. If you want to use default values, you can
-- override the result of @write@ either manually, or using automatically using
-- 'reset'.
write :: Writes exprs inserts => exprs -> inserts
write (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {} -> W $ case hfield exprs field of
      E expr -> Value expr


-- | @reset@ takes a 'Table' of 'Write's and, for each column which has a
-- default value, resets the value of that column to its default.
--
-- One example where this is useful is for any table that has a special @id@
-- column, which has a default value to draw a new value from a sequence. If
-- we want to make a copy of an existing row, but with a new @id@, we can run
-- the existing row through 'write', and then feed that to 'reset' to reset
-- the @id@ column.
reset :: Table Write a => a -> a
reset (toColumns -> as) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {defaulting} -> case hfield as field of
      W a -> W $ case defaulting of
        SNoDefault -> a
        SHasDefault -> Default
