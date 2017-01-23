{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Internal.Types where

import GHC.TypeLits (Symbol)
import Rel8.Internal.Expr (Expr)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

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
data SchemaInfo (name :: Symbol) (hasDefault :: HasDefault) t =
  SchemaInfo String


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
    tables.

    === __Example__

    @
    data Employee f =
      Employee { employeeName :: C f "employee_name" 'NoDefault Text) }
    @
-}
type family C f columnName hasDefault columnType :: * where
  C Expr _name _def t = Expr t
  C QueryResult _name _def t = t
  C Schema name hasDefault t = SchemaInfo name hasDefault t
  C Insert name 'HasDefault t = Default (Expr t)
  C Insert name 'NoDefault t = Expr t
  C Aggregate name _ t = Aggregate t


type family Anon f columnType :: * where
  Anon Expr t = Expr t
  Anon QueryResult t = t
  Anon Aggregate t = Aggregate t

--------------------------------------------------------------------------------
-- | Indicate whether or not a column has a default value. Used in conjunction
-- with 'C'
data HasDefault
  = HasDefault
  | NoDefault


--------------------------------------------------------------------------------
-- | Used to tag 'Expr's that are the result of aggregation
data Aggregate a =
  Aggregate (Maybe (O.AggrOp, [O.OrderExpr], O.AggrDistinct))
            O.PrimExpr

type role Aggregate representational
