{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'Table' type class.
module Rel8.Internal.Table where

import Data.Functor.Product
import Data.Profunctor
import Control.Applicative (liftA2)
import Control.Monad (replicateM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..), untag)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import GHC.Generics
       ((:*:)(..), Generic, K1(..), M1(..), Rep, from, to)
import Generics.OneLiner
       (ADTRecord, Constraints, For(..), createA, nullaryOp)
import qualified Opaleye.Aggregate as O
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
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
import Control.Monad.Zip

--------------------------------------------------------------------------------

data Some f where
  Some :: f a -> Some f

view :: ((a -> Identity b) -> (s -> Identity t)) -> s -> a
view = undefined

viewFrom :: ((a -> Identity b) -> (s -> Identity t)) -> a -> s
viewFrom = undefined

type family MkRowF a :: * -> * where
  MkRowF (M1 i c f) = MkRowF f
  MkRowF (a :*: b) = Product (MkRowF a) (MkRowF b)
  MkRowF (K1 i c) = RowF c

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
class (MonadZip (RowF expr), Traversable (RowF expr)) => Table expr haskell | expr -> haskell, haskell -> expr where
  type RowF expr :: * -> *
  type RowF expr = MkRowF (Rep expr)

  rowParser :: expr -> RowParser haskell

  expressions
    :: (Profunctor p, Functor f)
    => p (RowF expr (Some Expr)) (f (RowF expr (Some Expr))) -> p expr (f expr)

  ---------------------------------------------------------------------------------------

  default
    rowParser
      :: (Generic haskell, Generic expr, GTable (Rep expr) (Rep haskell))
      => expr -> RowParser haskell
  rowParser = fmap to . growParser . from

  default
    expressions
      :: ( Generic expr
         , GTable (Rep expr) (Rep haskell)
         , Functor f
         , Profunctor p
         , RowF expr ~ MkRowF (Rep expr)
         )
      => p (RowF expr (Some Expr)) (f (RowF expr (Some Expr))) -> p expr (f expr)
  expressions = dimap from (fmap to) . gexpressions


--------------------------------------------------------------------------------
class GTable expr haskell | expr -> haskell, haskell -> expr where
  growParser :: expr a -> RowParser (haskell a)
  gexpressions
    :: (Profunctor p, Functor f)
    => p (MkRowF expr (Some Expr)) (f (MkRowF expr (Some Expr)))
    -> p (expr ()) (f (expr ()))

instance GTable expr haskell => GTable (M1 i c expr) (M1 i c haskell) where
  growParser (M1 a) = M1 <$> growParser a
  gexpressions = dimap (\(M1 a) -> view gexpressions a) (fmap (M1 . viewFrom gexpressions))

instance (GTable le lh, GTable re rh) =>
         GTable (le :*: re) (lh :*: rh) where
  growParser (a :*: b) = liftA2 (:*:) (growParser a) (growParser b)
  gexpressions =
    dimap
      (\(a :*: b) -> Pair (view gexpressions a) (view gexpressions b))
      (fmap (\(Pair a b) -> viewFrom gexpressions a :*: viewFrom gexpressions b))

instance {-# OVERLAPPABLE #-} Table expr haskell => GTable (K1 i expr) (K1 i haskell) where
  growParser (K1 expr) = K1 <$> rowParser expr
  gexpressions = dimap (\(K1 a) -> view expressions a) (fmap (K1 . viewFrom expressions))

instance DBType a =>
         GTable (K1 i (Expr a)) (K1 i a) where
  growParser _ = K1 <$> field
  gexpressions =
    dimap
      (\(K1 a) -> Identity (Some a))
      (fmap (\(Identity (Some (Expr prim))) -> K1 (Expr prim)))

--------------------------------------------------------------------------------
-- Stock instances of 'Table'

-- | Any 'BaseTable' is a 'Table'.
instance {-# OVERLAPPABLE #-}
         ( BaseTable table
         , f ~ Expr
         , g ~ QueryResult
         , BaseTableF table ~ RowF (table Expr)
         ) =>
         Table (table f) (table g) where
  rowParser _ = parseBaseTable
  expressions = baseTableExprs

instance (Table a a', Table b b') =>
         Table (a, b) (a', b') where
  type RowF (a, b) = Product (RowF a) (RowF b)

  expressions =
    dimap
      (\(l, r) -> Pair (view expressions l) (view expressions r))
      (fmap (\(Pair l r) -> (viewFrom expressions l, viewFrom expressions r)))

  rowParser (l, r) = (,) <$> rowParser l <*> rowParser r

instance (Table a a', Table b b', Table c c') =>
         Table (a, b, c) (a', b', c') where
  type RowF (a, b, c) = Product (RowF (a, b)) (RowF c)

  expressions =
    dimap
      (\(a, b, c) ->
         Pair
           (Pair (view expressions a) (view expressions b))
           (view expressions c))
      (fmap
         (\(Pair (Pair a b) c) ->
            ( viewFrom expressions a
            , viewFrom expressions b
            , viewFrom expressions c)))

  rowParser (a, b, c) = (,,) <$> rowParser a <*> rowParser b <*> rowParser c

instance (Table a a', Table b b', Table c c', Table d d') =>
         Table (a, b, c, d) (a', b', c', d') where
  type RowF (a, b, c, d) = Product (RowF (a, b, c)) (RowF d)

  expressions =
    dimap
      (\(a, b, c, d) ->
         Pair
           (Pair
              (Pair (view expressions a) (view expressions b))
              (view expressions c))
           (view expressions d))
      (fmap
         (\(Pair (Pair (Pair a b) c) d) ->
            ( viewFrom expressions a
            , viewFrom expressions b
            , viewFrom expressions c
            , viewFrom expressions d)))

  rowParser (a, b, c, d) =
    (,,,) <$> rowParser a
          <*> rowParser b
          <*> rowParser c
          <*> rowParser d

instance (Table a a', Table b b', Table c c', Table d d', Table e e') =>
         Table (a, b, c, d, e) (a', b', c', d', e') where
  type RowF (a, b, c, d, e) = Product (RowF (a, b, c, d)) (RowF e)

  expressions =
    dimap
      (\(a, b, c, d, e) ->
         Pair
           (Pair
              (Pair
                 (Pair (view expressions a) (view expressions b))
                 (view expressions c))
              (view expressions d))
           (view expressions e))
      (fmap
         (\(Pair (Pair (Pair (Pair a b) c) d) e) ->
            ( viewFrom expressions a
            , viewFrom expressions b
            , viewFrom expressions c
            , viewFrom expressions d
            , viewFrom expressions e)))

  rowParser (a, b, c, d, e) =
    (,,,,) <$> rowParser a
           <*> rowParser b
           <*> rowParser c
           <*> rowParser d
           <*> rowParser e


--------------------------------------------------------------------------------
-- | Indicates that a given 'Table' might be @null@. This is the result of a
-- @LEFT JOIN@ between tables.
data MaybeTable row = MaybeTable (Expr (Maybe Bool)) row
  deriving (Functor)

-- | The result of a left/right join is a table, but the table may be entirely
-- @null@ sometimes.
instance (Table expr haskell) =>
         Table (MaybeTable expr) (Maybe haskell) where
  type RowF (MaybeTable expr) = Product Identity (RowF expr)

  expressions =
    dimap
      (\(MaybeTable tag row) -> Pair (view expressions tag) (view expressions row))
      (fmap
         (\(Pair tag row) ->
            MaybeTable (viewFrom expressions tag) (viewFrom expressions row)))

  rowParser (MaybeTable _ row)  = do
    isNull' <- field
    if fromMaybe True isNull'
      then Nothing <$ replicateM_ (length (view expressions row)) (field :: RowParser (Maybe ()))
      else fmap Just (rowParser row)

  -- traverseBinary f (MaybeTable tagL l, MaybeTable tagR r) =
  --   MaybeTable <$> traverseBinary f (tagL, tagR) <*> traverseBinary f (l, r)

-- | Project an expression out of a 'MaybeTable', preserving the fact that this
-- column might be @null@. Like field selection.
--
-- It may be helpful to remember this operator by the mneumonic - '$' on the left
-- means function on the left, '?' on the right means 'MaybeTable' on the right.
infixl 4 $?
($?) :: ToNullable b maybeB => (a -> Expr b) -> MaybeTable a -> Expr maybeB
f $? MaybeTable _ x = toNullable (f x)

-- | Check if a 'MaybeTable' is a @NULL@ row. Usually this means a @LEFT JOIN@
-- that did match any rows.
isTableNull :: MaybeTable a -> Expr Bool
isTableNull (MaybeTable tag _) = nullable (lit True) (\_ -> lit False) tag

--------------------------------------------------------------------------------
-- | Eliminate 'Maybe' from the type of an 'Expr'. Like 'maybe' for Haskell
-- values.
nullable
  :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
nullable a f b =
  columnToExpr
    (O.matchNullable
       (exprToColumn a)
       (exprToColumn . f . columnToExpr)
       (exprToColumn b))


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
  type RowF (Expr a) = Identity
  expressions = dimap (Identity . Some) (fmap (\(Identity (Some (Expr prim))) -> Expr prim))
  rowParser _ = fmap Col field

--------------------------------------------------------------------------------
traversePrimExprs
  :: (Applicative f, Table expr haskell)
  => (O.PrimExpr -> f O.PrimExpr) -> expr -> f expr
traversePrimExprs f =
  fmap (viewFrom expressions) .
  traverse (\(Some (Expr prim)) -> Some . Expr <$> f prim) . view expressions

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
  type BaseTableF table :: * -> *
  type BaseTableF table = MkRowF (Rep (table Expr))

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
  baseTableExprs
    :: (Profunctor p, Functor f)
    => p (BaseTableF table (Some Expr)) (f (BaseTableF table (Some Expr)))
    -> p (table Expr) (f (table Expr))

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
    baseTableExprs
      :: ( Generic (table Expr)
         , GTable (Rep (table Expr)) (Rep (table QueryResult))
         , Functor f
         , Profunctor p
         , RowF (table Expr) ~ MkRowF (Rep (table Expr))
         )
      => p (RowF (table Expr) (Some Expr)) (f (RowF (table Expr) (Some Expr)))
      -> p (table Expr) (f (table Expr))
  baseTableExprs = dimap from (fmap to) . gexpressions

  default
    traverseBaseTableAggregates
      :: ( GTraverseAggregator (Rep (table Aggregate)) (Rep (table Expr))
         , Generic (table Expr)
         , Generic (table Aggregate)
         )
      => O.Aggregator (table Aggregate) (table Expr)
  traverseBaseTableAggregates = dimap from to gaggregator

  default
    updateWriter
      :: ( ADTRecord (table Expr)
         , ADTRecord (table Schema)
         , Writer Expr (Rep (table Schema)) (Rep (table Expr))
         )
      => O.Writer (table Expr) a
  updateWriter =
    case lmap from (columnWriter (Proxy @Expr) (from (tableSchema @table))) of
      O.Writer f -> O.Writer f

  default
    insertWriter :: ( ADTRecord (table Schema)
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
-- __Example__
--
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
instance {-# OVERLAPPABLE #-}
  (BaseTable table, f ~ Aggregate, g ~ Expr) => AggregateTable (table f) (table g) where
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


--------------------------------------------------------------------------------
-- | Evaluate aggregation over a query. The 'AggregateTable' constraint
-- requires that all columns in each row must be grouped or aggregated.
aggregate
  :: AggregateTable table result
  => O.Query table -> O.Query result
aggregate = O.aggregate traverseAggregates
