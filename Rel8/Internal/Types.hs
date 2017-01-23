module Rel8.Internal.Types where

--------------------------------------------------------------------------------
-- | Interpret a 'Table' as Haskell values.
data QueryResult column


--------------------------------------------------------------------------------
-- Used to indicate that we will be inserting values. If columns are marked as
-- having a default value, it will be possible to use 'Default' to let the
-- database generate a value.
data Insert a


--------------------------------------------------------------------------------
-- | Used internal to reflect the schema of a table from types into values.
data Schema a


--------------------------------------------------------------------------------
-- | When inserting into tables, some columns may be marked as allowing
-- defaults (indicated by the argument @'HasDefault@ to 'C'). If this is the
-- case, you will need to supply @Default Expr@s as column values. 'Default'
-- indicates that you are either supplying a value explictly
-- (with 'OverrideDefault'), or you wish the databse to provide a value (with
-- 'InsertDefault'). The latter is useful for automatically generated primary
-- keys, or timestamps.
data Default a
  = OverrideDefault a
  | InsertDefault
