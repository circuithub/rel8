{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Internal.Types where

import Rel8.Internal.Expr
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

--------------------------------------------------------------------------------
-- | Interpret a 'Table' as Haskell values.
data QueryResult column


--------------------------------------------------------------------------------
-- Used to indicate that we will be inserting values. If columns are marked as
-- having a default value, it will be possible to use 'Default' to let the
-- database generate a value.
data Insert a = InsertExpr (Expr a)


--------------------------------------------------------------------------------
-- | Used internal to reflect the schema of a table from types into values.
data SchemaInfo a =
  SchemaInfo String
  deriving (Show)


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

    === Example

    @
    data Employee f =
      Employee { employeeName :: C f "employee_name" 'NoDefault Text) }
    @
-}
type family C f columnName hasDefault columnType :: * where
  C Expr _name _def t = Expr t
  C QueryResult _name _def t = t
  C SchemaInfo name hasDefault t = SchemaInfo '(name, hasDefault, t)
  C Insert name 'HasDefault t = Default (Expr t)
  C Insert name 'NoDefault t = Expr t
  C Aggregate name _ t = Aggregate t

-- | @Anon@ can be used to define columns like 'C', but does not contain the
-- extra metadata needed to define a 'BaseTable' instance. The main use of
-- 'Anon' is to define "anonymous" tables that represent the intermediate
-- parts of a query.
--
-- === Example
--
-- @
-- data EmployeeAndManager f = EmployeeAndManager
--   { employee :: Anon f Text
--   , manager :: Anon f Text
--   }
--
-- employeeAndManager :: Query (EmployeeAndManager Expr)
-- employeeAndManager = proc _ -> do
--   employee <- queryTable -< ()
--   manager <- queryTable -< ()
--   where_ -< employeeManager employee ==. managerId manager
--   id -< EmployeeAndManager { employee = employeeName employee
--                            , manager = managerName manager
--                            }
-- @
type family Anon f columnType :: * where
  Anon Expr t = Expr t
  Anon QueryResult t = t
  Anon Aggregate t = Aggregate t

newtype Limit f = Limit
  { runLimit :: forall a. f a
  }

data Colimit f where
  Colimit :: f a -> Colimit f

data Interpretation f where
  AsExpr :: Interpretation Expr
  AsSchemaInfo :: Interpretation SchemaInfo
  AsInsert :: Interpretation Insert

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
