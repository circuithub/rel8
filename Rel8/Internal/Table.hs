{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'Table' type class.
module Rel8.Internal.Table where

import Data.Profunctor.Product ((***!))
import Control.Applicative (Const(..), liftA2)
import Control.Monad (replicateM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import Data.Profunctor (dimap, lmap)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), proxy, untag, retag)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import GHC.Generics
       ((:*:)(..), Generic, K1(..), M1(..), Rep, from, to)
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
import Rel8.Internal.DBType
import Rel8.Internal.Expr
import Rel8.Internal.Generic
import Rel8.Internal.Types
import qualified Opaleye.Internal.Aggregate as O

--------------------------------------------------------------------------------

-- | 'Table' @expr haskell@ specifies that the @expr@ contains one or more
-- 'Expr' columns, and when this table is queried using 'select' it returns
-- the type @haskell@.
--
-- 'Table's are not necessarily concrete tables within a database. For example,
-- the join of two 'Table's (as witnessed by tuple construction) is itself a
-- 'Table' - but cannot be inserted as it doesn't belong to any base table.
--
-- You should not need to define your own instances of 'Table' in idiomatic
-- @rel8@ usage - all 'BaseTable's are 'Table's, and the tupling of two (or
-- more) 'Table's is also a 'Table'.
class Table expr haskell | expr -> haskell, haskell -> expr where
  rowParser :: RowParser haskell

  -- | The amount of columns in this table. Needed by 'MaybeTable' in order
  -- to parse a series of @null@ values.
  columnCount :: Tagged haskell Int

  traversePrimExprs :: Applicative f => (O.PrimExpr -> f O.PrimExpr) -> expr -> f expr

  ---------------------------------------------------------------------------------------

  default
    rowParser
      :: (Generic haskell, GTable (Rep expr) (Rep haskell))
      => RowParser haskell
  rowParser = to <$> growParser

  default
    columnCount
      :: (Generic haskell, GTable (Rep expr) (Rep haskell))
      => Tagged haskell Int
  columnCount = retag (gcolumnCount @_ @(Rep haskell))

  default
    traversePrimExprs
      :: (Generic expr, GTable (Rep expr) (Rep haskell), Applicative f)
      => (O.PrimExpr -> f O.PrimExpr) -> expr -> f expr
  traversePrimExprs f = fmap to . gtraversePrimExprs f . from


--------------------------------------------------------------------------------
class GTable expr haskell | expr -> haskell, haskell -> expr where
  growParser :: RowParser (haskell a)
  gcolumnCount :: Tagged (haskell a) Int
  gtraversePrimExprs
    :: Applicative f
    => (O.PrimExpr -> f O.PrimExpr) -> expr a -> f (expr a)

instance GTable expr haskell => GTable (M1 i c expr) (M1 i c haskell) where
  growParser = M1 <$> growParser
  gcolumnCount = retag (gcolumnCount @_ @haskell)
  gtraversePrimExprs f (M1 a) = M1 <$> gtraversePrimExprs f a

instance (GTable le lh, GTable re rh) =>
         GTable (le :*: re) (lh :*: rh) where
  growParser = liftA2 (:*:) growParser growParser
  gcolumnCount = retag (gcolumnCount @_ @lh) + retag (gcolumnCount @_ @rh)
  gtraversePrimExprs f (l :*: r) =
    liftA2 (:*:) (gtraversePrimExprs f l) (gtraversePrimExprs f r)

instance Table expr haskell => GTable (K1 i expr) (K1 i haskell) where
  growParser = K1 <$> rowParser
  gcolumnCount = retag (columnCount @_ @haskell)
  gtraversePrimExprs f (K1 a) = K1 <$> traversePrimExprs f a

--------------------------------------------------------------------------------
-- Stock instances of 'Table'

-- | Any 'BaseTable' is a 'Table'.
instance {-# OVERLAPPABLE #-} BaseTable table => Table (table Expr) (table QueryResult) where
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

-- | The result of a left/right join is a table, but the table may be entirely
-- @null@ sometimes.
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

-- | Single 'Expr'essions are tables, but the result will be wrapped in 'Col'.
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
-- | A 'BaseTable' is a table that is specified directly in a relational
-- database schema with @CREATE TABLE@. You introduce 'BaseTables' by defining
-- Haskell records parameterised over some functor @f@, and then use 'C' to
-- define individual columns. Finally, derive 'Generic' and provide a minimal
-- 'BaseTable' instance.
--
-- === Example
--
-- @
-- data Part f =
--   Part { partId     :: 'C' f \"PID\" ''HasDefault' Int
--        , partName   :: 'C' f \"PName\" ''NoDefault' Text
--        , partColor  :: 'C' f \"Color\" ''NoDefault' Int
--        } deriving (Generic)
--
-- instance 'BaseTable' Part where tableName = "part"
-- @
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
  traverseBaseTableAggregates :: O.Aggregator (table Aggregate) (table Expr)

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
    traverseBaseTableAggregates
      :: ( GTraverseAggregator (Rep (table Aggregate)) (Rep (table Expr))
         , Generic (table Expr)
         , Generic (table Aggregate)
         )
      => O.Aggregator (table Aggregate) (table Expr)
  traverseBaseTableAggregates = dimap from to gaggregator

  default
    updateWriter :: ( ADTRecord (table Expr)
                    , ADTRecord (table Schema)
                    , Constraints (table Schema) WitnessSchema
                    , Writer Expr (Rep (table Schema)) (Rep (table Expr))
                    , Generic (table Expr)
                    )
                 => O.Writer (table Expr) a
  updateWriter =
    case lmap from (columnWriter (Proxy @Expr) (from (tableSchema @table))) of
      O.Writer f -> O.Writer f

  default
    insertWriter :: ( ADTRecord (table Expr)
                    , ADTRecord (table Schema)
                    , Constraints (table Schema) WitnessSchema
                    , Writer Insert (Rep (table Schema)) (Rep (table Insert))
                    , Generic (table Insert)
                    )
                 => O.Writer (table Insert) a
  insertWriter =
    case lmap from (columnWriter (Proxy @Insert) (from (tableSchema @table))) of
      O.Writer f -> O.Writer f

--------------------------------------------------------------------------------
viewTable :: BaseTable table => table Expr
viewTable =
  runIdentity
    (traverseSchema (Identity . Expr . O.BaseTableAttrExpr) tableSchema)


--------------------------------------------------------------------------------
-- | Query all rows in a table. Equivalent to @SELECT * FROM table@.
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
-- | 'AggregateTable' is used to demonstrate that a table only contains
-- aggregation or @GROUP BY@ expressions. If you wish to use your own records
-- for aggregation results, parameterise the record over @f@, use 'Anon' to
-- specify the columns, and then generically derive 'AggregateTable':
--
-- *** __Example__
-- @
-- data UserInfo f = UserInfo
--   { userCount :: Anon f Int64
--   , latestLogin :: Anon f UTCTime
--   } deriving (Generic)
-- instance AggregateTable (UserInfo Aggregate) (UserInfo Expr)
-- @

class AggregateTable columns result | columns -> result, result -> columns where
  traverseAggregates :: O.Aggregator columns result

  default
    traverseAggregates
      :: ( GTraverseAggregator (Rep columns) (Rep result)
         , Generic columns
         , Generic result
         )
      => O.Aggregator columns result
  traverseAggregates = dimap from to gaggregator

-- | A single column aggregates to a single expression.
instance AggregateTable (Aggregate a) (Expr a) where
  traverseAggregates =
    O.Aggregator (O.PackMap (\f (Aggregate a b) -> fmap Expr (f (a, b))))

instance (AggregateTable a1 b1, AggregateTable a2 b2) =>
         AggregateTable (a1, a2) (b1, b2) where
  traverseAggregates = traverseAggregates ***! traverseAggregates

-- | Any base table can be aggregated, provided you specify how to aggregate
-- each column.
instance BaseTable table => AggregateTable (table Aggregate) (table Expr) where
  traverseAggregates = traverseBaseTableAggregates


--------------------------------------------------------------------------------
class GTraverseAggregator aggregator expr | aggregator -> expr where
  gaggregator
    :: O.Aggregator (aggregator x) (expr y)

instance (GTraverseAggregator aggregator expr) =>
         GTraverseAggregator (M1 i c aggregator) (M1 i c expr) where
  gaggregator = dimap (\(M1 a) -> a) M1 gaggregator

instance ( GTraverseAggregator fAggregator fExpr
         , GTraverseAggregator gAggregator gExpr
         ) =>
         GTraverseAggregator (fAggregator :*: gAggregator) (fExpr :*: gExpr) where
  gaggregator =
    dimap (\(a :*: b) -> (a, b)) (uncurry (:*:)) (gaggregator ***! gaggregator)

instance AggregateTable a b => GTraverseAggregator (K1 i a) (K1 i b) where
  gaggregator = dimap (\(K1 a) -> a) K1 traverseAggregates
