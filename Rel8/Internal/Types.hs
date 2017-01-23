{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Internal.Types where

import GHC.TypeLits (Symbol)
import Data.Tagged (Tagged(..))
import Rel8.Internal.Aggregate (Aggregate)
import Rel8.Internal.Expr (Expr)

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


--------------------------------------------------------------------------------
{-| All metadata about a column in a table.

    'C' is used to specify information about individual columns in base
    tables. While it is defined as a record, you construct 'Column's at the
    type level where record syntax is unfortunately not available.

    === __Example__

    @
    data Employee f =
      Employee { employeeName :: C f ('Column "employee_name" 'NoDefault 'NotNullable 'PGText) }
    @
-}
type family C (f :: * -> *) (columnName :: Symbol) (hasDefault :: HasDefault) (columnType :: t) :: * where
  C Expr _name _def t = Expr t
  C QueryResult _name _def t = t
  C Schema name _def _t = Tagged name String
  C Insert name 'HasDefault t = Default (Expr t)
  C Insert name 'NoDefault t = Expr t
  C Aggregate name _ t = Aggregate t


--------------------------------------------------------------------------------
-- | Indicate whether or not a column has a default value.
data HasDefault
  = HasDefault
  | NoDefault


--------------------------------------------------------------------------------
-- | Indicate whether or not a column can take default values.
data Nullable
  = Nullable
  | NotNullable
