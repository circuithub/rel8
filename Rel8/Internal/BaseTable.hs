{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Rel8.Internal.BaseTable where

import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import GHC.Generics
       (Generic, Rep, K1(..), M1(..), (:*:)(..), from, to)
import GHC.TypeLits (symbolVal, KnownSymbol)
import Generics.OneLiner
       (ADTRecord, Constraints, For(..), nullaryOp)
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.TableMaker as O
import qualified Opaleye.Table as O hiding (required)
import Prelude hiding (not, (.), id)
import Rel8.Internal.Expr
import Rel8.Internal.Table
import Rel8.Internal.Types

--------------------------------------------------------------------------------
-- TODO Unsure if we want to assume this type of table

-- | 'BaseTable' @name record@ specifies that there is a table named @name@, and
-- the record type @record@ specifies the columns of that table.
class (KnownSymbol name, Table (table Expr) (table QueryResult)) =>
      BaseTable name table | table -> name where
  -- | Query all rows in a table
  queryTable :: O.Query (table Expr)
  queryTable =
    O.queryTableExplicit
      (O.ColumnMaker (O.PackMap traversePrimExprs))
      tableDefinition

  tableDefinition :: O.Table (table Insert) (table Expr)
  default
    tableDefinition :: ( ADTRecord (table Expr)
                       , ADTRecord (table Schema)
                       , Constraints (table Schema) WitnessSchema
                       , InferBaseTableAttrExpr (Rep (table Schema)) (Rep (table Expr))
                       , Writer (Rep (table Schema)) (Rep (table Insert))
                       , Generic (table Insert)
                       )
                    => O.Table (table Insert) (table Expr)
  tableDefinition =
    O.Table
         (symbolVal (Proxy @name))
         (O.TableProperties
            (case lmap from (columnWriter (from tableSchema)) of
               O.Writer f -> O.Writer f)
            (O.View (to (baseTableAttrExpr (from tableSchema)))))
    where
      tableSchema :: table Schema
      tableSchema = nullaryOp (For :: For WitnessSchema) schema

  -- TODO Would really like to reconcile this with tableDefinition
  tableDefinitionUpdate :: O.Table (table Expr) (table Expr)
  default
    tableDefinitionUpdate :: ( ADTRecord (table Expr)
                             , ADTRecord (table Schema)
                             , Constraints (table Schema) WitnessSchema
                             , InferBaseTableAttrExpr (Rep (table Schema)) (Rep (table Expr))
                             , Writer (Rep (table Schema)) (Rep (table Expr))
                             )
                          => O.Table (table Expr) (table Expr)
  tableDefinitionUpdate =
    O.Table
         (symbolVal (Proxy @name))
         (O.TableProperties
            (case lmap from (columnWriter (from tableSchema)) of
               O.Writer f -> O.Writer f)
            (O.View (to (baseTableAttrExpr (from tableSchema)))))
    where
      tableSchema :: table Schema
      tableSchema = nullaryOp (For :: For WitnessSchema) schema


--------------------------------------------------------------------------------
-- | Witness the schema definition for table columns.
class WitnessSchema a where
  schema :: a

instance KnownSymbol name =>
         WitnessSchema (Tagged name String) where
  schema = Tagged (symbolVal (Proxy @name))


--------------------------------------------------------------------------------
-- | Map a schema definition into a set of expressions that would select those
-- columns.
class InferBaseTableAttrExpr schema expr where
  baseTableAttrExpr :: schema a -> expr a

instance (InferBaseTableAttrExpr schema expr) =>
         InferBaseTableAttrExpr (M1 i c schema) (M1 i c expr) where
  baseTableAttrExpr (M1 s) = M1 (baseTableAttrExpr s)

instance ( InferBaseTableAttrExpr fSchema fExpr
         , InferBaseTableAttrExpr gSchema gExpr
         ) =>
         InferBaseTableAttrExpr (fSchema :*: gSchema) (fExpr :*: gExpr) where
  baseTableAttrExpr (l :*: r) = baseTableAttrExpr l :*: baseTableAttrExpr r

instance InferBaseTableAttrExpr (K1 i (Tagged name String)) (K1 i (Expr a)) where
  baseTableAttrExpr (K1 (Tagged name)) = K1 (Expr (O.BaseTableAttrExpr name))


--------------------------------------------------------------------------------
-- | Form 'O.Writer's from a schema specification
class Writer schema expr where
  columnWriter :: schema a -> O.Writer (expr a) ()

instance (Writer schema expr) =>
         Writer (M1 i c schema) (M1 i c expr) where
  columnWriter (M1 s) = lmap (\(M1 a) -> a) (columnWriter s)

instance (Writer fSchema fExpr, Writer gSchema gExpr) =>
         Writer (fSchema :*: gSchema) (fExpr :*: gExpr) where
  columnWriter (l :*: r) =
    dimap (\(l' :*: r') -> (l', r')) fst (columnWriter l ***! columnWriter r)

instance Writer (K1 i (Tagged name String)) (K1 i (Expr a)) where
  columnWriter (K1 (Tagged name)) =
    dimap
      (\(K1 expr) -> exprToColumn expr)
      (const ())
      (O.required name)

instance Writer (K1 i (Tagged name String)) (K1 i (Default (Expr a))) where
  columnWriter (K1 (Tagged name)) =
    dimap
      (\(K1 def) ->
         case def of
           InsertDefault -> O.Column O.DefaultInsertExpr
           OverrideDefault expr -> exprToColumn expr)
      (const ())
      (O.required name)
