{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Rel8.Internal.BaseTable where

import Control.Applicative (liftA2)
import Data.Functor.Identity
import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), untag)
import GHC.Generics
       (Generic, Rep, K1(..), M1(..), (:*:)(..), from, to)
import GHC.TypeLits (symbolVal, KnownSymbol)
import Generics.OneLiner
       (ADTRecord, Constraints, For(..), gtraverse, nullaryOp)
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.TableMaker as O
import qualified Opaleye.Table as O hiding (required)
import Prelude hiding (not, id)
import Rel8.Internal.Expr
import Rel8.Internal.Table
import Rel8.Internal.Types
import Rel8.Internal.Generic
import Rel8.Internal.Aggregate

--------------------------------------------------------------------------------
-- TODO Unsure if we want to assume this type of table

-- | 'BaseTable' @name record@ specifies that there is a table named @name@, and
-- the record type @record@ specifies the columns of that table.
class (Table (table Expr) (table QueryResult)) => BaseTable table where
  -- | The name of this table in the database. You can use the 'FromString'
  -- instance for 'Tagged' to simply write
  -- @tableName = "employees"@, for example.
  tableName :: Tagged table String

  -- | Witness the schema of a table at the value level.
  tableSchema :: table Schema

  -- | Traverse over all column names in a schema, converting to 'Expr's that
  -- would select those columns.
  traverseSchema
    :: Applicative f
    => (forall a. String -> f (Expr a)) -> table Schema -> f (table Expr)

  -- | Traverse over all primitive expressions in a table of expressions.
  traverseExprs
    :: Applicative f
    => (O.PrimExpr -> f O.PrimExpr) -> table Expr -> f (table Expr)

  -- | Traverse over all aggregates in a table of aggregations, converting them
  -- to the expressions that refer to aggregation results.
  traverseAggregate
    :: Applicative f
    => (forall a. Aggregate a -> f (Expr a)) -> table Aggregate -> f (table Expr)

  insertWriter :: O.Writer (table Insert) a

  updateWriter :: O.Writer (table Expr) a

  ------------------------------------------------------------------------------

  default
    tableSchema
      :: (ADTRecord (table Schema), Constraints (table Schema) WitnessSchema)
      => table Schema
  tableSchema = nullaryOp (For :: For WitnessSchema) schema

  default
    traverseSchema
      :: ( GTraverseSchema (Rep (table Schema)) (Rep (table Expr))
         , Generic (table Schema)
         , Generic (table Expr)
         , Applicative f
         )
      => (forall a. String -> f (Expr a)) -> table Schema -> f (table Expr)
  traverseSchema f = fmap to . gtraverseSchema f . from

  default
    traverseExprs
      :: ( ADTRecord (table Expr)
         , Constraints (table Expr) MapPrimExpr
         , Applicative f
         )
      => (O.PrimExpr -> f O.PrimExpr) -> table Expr -> f (table Expr)
  traverseExprs f = gtraverse (For :: For MapPrimExpr) (mapPrimExpr f)

  default
    updateWriter :: ( ADTRecord (table Expr)
                    , ADTRecord (table Schema)
                    , Constraints (table Schema) WitnessSchema
                    , Writer (Rep (table Schema)) (Rep (table Insert))
                    , Generic (table Insert)
                    )
                 => O.Writer (table Insert) a
  updateWriter =
    case lmap from (columnWriter (from (tableSchema @table))) of
      O.Writer f -> O.Writer f

  default
    insertWriter :: ( ADTRecord (table Expr)
                    , ADTRecord (table Schema)
                    , Constraints (table Schema) WitnessSchema
                    , Writer (Rep (table Schema)) (Rep (table Expr))
                    )
                 => O.Writer (table Expr) a
  insertWriter =
    case lmap from (columnWriter (from (tableSchema @table))) of
      O.Writer f -> O.Writer f

--------------------------------------------------------------------------------
viewTable :: BaseTable table => table Expr
viewTable =
  runIdentity
    (traverseSchema (Identity . Expr . O.BaseTableAttrExpr) tableSchema)


--------------------------------------------------------------------------------
-- | Query all rows in a table
queryTable :: BaseTable table => O.Query (table Expr)
queryTable =
  O.queryTableExplicit
    (O.ColumnMaker (O.PackMap traversePrimExprs))
    tableDefinition

--------------------------------------------------------------------------------
tableDefinition
  :: forall table.
     BaseTable table
  => O.Table (table Insert) (table Expr)
tableDefinition =
  O.Table
    (untag @table tableName)
    (O.TableProperties insertWriter (O.View viewTable))

tableDefinitionUpdate
  :: forall table.
     BaseTable table
  => O.Table (table Expr) (table Expr)
tableDefinitionUpdate =
  O.Table
    (untag @table tableName)
    (O.TableProperties updateWriter (O.View viewTable))

--------------------------------------------------------------------------------
-- | Witness the schema definition for table columns.
class WitnessSchema a where
  schema :: a

instance KnownSymbol name =>
         WitnessSchema (Tagged name String) where
  schema = Tagged (symbolVal (Proxy @name))


--------------------------------------------------------------------------------
class GTraverseSchema schema expr where
  gtraverseSchema
    :: Applicative f
    => (forall a. String -> f (Expr a)) -> schema x -> f (expr y)

instance (GTraverseSchema schema expr) =>
         GTraverseSchema (M1 i c schema) (M1 i c expr) where
  gtraverseSchema f (M1 a) = M1 <$> gtraverseSchema f a

instance (GTraverseSchema fSchema fExpr, GTraverseSchema gSchema gExpr) =>
         GTraverseSchema (fSchema :*: gSchema) (fExpr :*: gExpr) where
  gtraverseSchema f (l :*: r) =
    liftA2 (:*:) (gtraverseSchema f l) (gtraverseSchema f r)

instance GTraverseSchema (K1 i (Tagged name String)) (K1 i (Expr a)) where
  gtraverseSchema f (K1 (Tagged a)) = K1 <$> f a


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
