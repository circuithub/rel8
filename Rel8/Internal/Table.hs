{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'Table' type class.
module Rel8.Internal.Table where

import Control.Applicative (Const(..))
import Control.Monad (replicateM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import Data.Profunctor (lmap)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), proxy, untag)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import GHC.Generics
       (Generic, Rep, from, to)
import Generics.OneLiner
       (ADTRecord, Constraints, For(..), createA, gtraverse, nullaryOp)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.TableMaker as O
import qualified Opaleye.Internal.Unpackspec as O
import qualified Opaleye.Table as O hiding (required)
import Prelude hiding (not)
import Prelude hiding (not, id)
import Rel8.DBType
import Rel8.Internal.Aggregate
import Rel8.Internal.Expr
import Rel8.Internal.Generic
import Rel8.Internal.Types

--------------------------------------------------------------------------------

-- | 'Table' @expr haskell@ specifies that the @expr@ contains one or more
-- 'Expr' columns, and when this table is queried using 'select' it returns
-- the type @haskell@.
--
-- 'Table's are not necessarily concrete tables within a database. For example,
-- the join of two 'Table's (as witness by tuple construction) is itself a
-- 'Table'.

class Table expr haskell | expr -> haskell, haskell -> expr where
  rowParser :: RowParser haskell

  -- | The amount of columns in this table. Needed by 'MaybeTable' in order
  -- to parse a series of @null@ values.
  columnCount :: Tagged haskell Int

  traversePrimExprs :: Applicative f => (O.PrimExpr -> f O.PrimExpr) -> expr -> f expr


--------------------------------------------------------------------------------
-- Stock instances of 'Table'

-- | Any base table is a 'Table'.
instance BaseTable table => Table (table Expr) (table QueryResult) where
  columnCount =
    Tagged (getSum (getConst (traverseSchema (const (Const (Sum 1))) (tableSchema @table))))

  rowParser = parseBaseTable

  traversePrimExprs = traverseBaseTableExprs

instance (Table a a', Table b b') =>
         Table (a, b) (a', b') where
  columnCount = Tagged
    $ proxy columnCount (Proxy @a')
    + proxy columnCount (Proxy @b')

  traversePrimExprs f (a, b) =
    (,) <$> traversePrimExprs f a
        <*> traversePrimExprs f b

  rowParser =
    (,) <$> rowParser
        <*> rowParser

instance (Table a a', Table b b', Table c c') =>
         Table (a, b, c) (a', b', c') where
  columnCount = Tagged
    $ proxy columnCount (Proxy @a')
    + proxy columnCount (Proxy @b')
    + proxy columnCount (Proxy @c')

  traversePrimExprs f (a, b, c) =
    (,,) <$> traversePrimExprs f a
         <*> traversePrimExprs f b
         <*> traversePrimExprs f c

  rowParser =
    (,,) <$> rowParser
         <*> rowParser
         <*> rowParser

instance (Table a a', Table b b', Table c c', Table d d') =>
         Table (a, b, c, d) (a', b', c', d') where
  columnCount = Tagged
    $ proxy columnCount (Proxy @a')
    + proxy columnCount (Proxy @b')
    + proxy columnCount (Proxy @c')
    + proxy columnCount (Proxy @d')

  traversePrimExprs f (a, b, c, d) =
    (,,,) <$> traversePrimExprs f a
          <*> traversePrimExprs f b
          <*> traversePrimExprs f c
          <*> traversePrimExprs f d

  rowParser =
    (,,,) <$> rowParser
          <*> rowParser
          <*> rowParser
          <*> rowParser

instance (Table a a', Table b b', Table c c', Table d d', Table e e') =>
         Table (a, b, c, d, e) (a', b', c', d', e') where
  columnCount = Tagged
    $ proxy columnCount (Proxy @a')
    + proxy columnCount (Proxy @b')
    + proxy columnCount (Proxy @c')
    + proxy columnCount (Proxy @d')
    + proxy columnCount (Proxy @e')

  traversePrimExprs f (a, b, c, d, e) =
    (,,,,) <$> traversePrimExprs f a
           <*> traversePrimExprs f b
           <*> traversePrimExprs f c
           <*> traversePrimExprs f d
           <*> traversePrimExprs f e

  rowParser =
    (,,,,) <$> rowParser
           <*> rowParser
           <*> rowParser
           <*> rowParser
           <*> rowParser


--------------------------------------------------------------------------------
-- | Indicates that a given 'Table' might be @null@. This is the result of a
-- @LEFT JOIN@ between tables.
data MaybeTable row = MaybeTable (Expr Bool) row
  deriving (Functor)

instance (Table expr haskell) =>
         Table (MaybeTable expr) (Maybe haskell) where
  columnCount = Tagged
    $ 1 + proxy columnCount (Proxy @haskell)

  traversePrimExprs f (MaybeTable (Expr tag) row) =
    MaybeTable <$> (Expr <$> f tag) <*> traversePrimExprs f row

  rowParser = do
    isNull' <- field
    if fromMaybe True isNull'
      then Nothing <$ replicateM_ (proxy columnCount (Proxy @haskell)) (field :: RowParser (Maybe ()))
      else fmap Just rowParser


--------------------------------------------------------------------------------
{- | A one column 'Table' of type @a@. This type is required for queries that
   return only one column (for reasons of preserving type inference). It can
   also be used to build "anonymous" tables, by joining multiple tables with
   tupling.

   === Example: Querying a single column

   @
   data TestTable f = TestTable { col :: Col f "col" 'NoDefault Int}

   oneCol :: Stream (Of (Col Int))
   oneCol = select connection $ testColumn <$> queryTable
   @

   === Example: Building tables out of single columns

   @
   data T1 f = TestTable { col1 :: Col f "col" 'NoDefault Int}
   data T2 f = TestTable { col2 :: Col f "col" 'NoDefault Bool}

   q :: Stream (Of (Col Int, Col Bool))
   q = select connection $ proc () -> do
     t1 <- queryTable -< ()
     t2 <- queryTable -< ()
     returnA -< (col1 t1, col2 t2)
   @
-}
newtype Col a = Col { unCol :: a }
  deriving (Show, ToJSON, FromJSON, Read, Eq, Ord)

instance (DBType a) =>
         Table (Expr a) (Col a) where
  columnCount = Tagged 1
  traversePrimExprs f (Expr a) = Expr <$> f a
  rowParser = fmap Col field


--------------------------------------------------------------------------------

-- | Project an expression out of a 'MaybeTable', preserving the fact that this
-- column might be @null@.
(?) :: ToNullable b maybeB => MaybeTable a -> (a -> Expr b) -> Expr maybeB
MaybeTable _ row ? f = toNullable (f row)


--------------------------------------------------------------------------------
unpackColumns :: Table expr haskell => O.Unpackspec expr expr
unpackColumns = O.Unpackspec (O.PackMap traversePrimExprs)


--------------------------------------------------------------------------------
-- | 'BaseTable' @name record@ specifies that there is a table named @name@, and
-- the record type @record@ specifies the columns of that table.
class (Table (table Expr) (table QueryResult)) => BaseTable table where
  -- | The name of this table in the database. You can use the 'FromString'
  -- instance for 'Tagged' to simply write
  -- @tableName = "employees"@, for example.
  tableName :: Tagged table String

  -- | Witness the schema of a table at the value level.
  tableSchema :: table Schema

  -- | Parse query results for this table.
  parseBaseTable :: RowParser (table QueryResult)

  -- | Traverse over all column names in a schema, converting to 'Expr's that
  -- would select those columns.
  traverseSchema
    :: Applicative f
    => (forall a. String -> f (Expr a)) -> table Schema -> f (table Expr)

  -- | Traverse over all primitive expressions in a table of expressions.
  traverseBaseTableExprs
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
    parseBaseTable :: ( ADTRecord (table QueryResult)
                      , Constraints (table QueryResult) FromField
                      )
                   => RowParser (table QueryResult)
  parseBaseTable =
    head (getCompose (createA (For :: For FromField) (Compose [field])))

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
    traverseBaseTableExprs
      :: ( ADTRecord (table Expr)
         , Constraints (table Expr) MapPrimExpr
         , Applicative f
         )
      => (O.PrimExpr -> f O.PrimExpr) -> table Expr -> f (table Expr)
  traverseBaseTableExprs f = gtraverse (For :: For MapPrimExpr) (mapPrimExpr f)

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
